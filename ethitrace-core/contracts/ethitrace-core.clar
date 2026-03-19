;; EthiTrace - Supply Chain Transparency Contract

;; Implements:
;;   - Stakeholder registration with token commitment
;;   - Product registration and ethical history tracking
;;   - Violation reporting with graduated severity levels
;;   - Reputation scoring system
;;   - Ethical Debt offset mechanism via sustainability projects
;;   - Reward/fee reduction for ethical behaviour

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-ALREADY-REGISTERED   (err u101))
(define-constant ERR-NOT-REGISTERED       (err u102))
(define-constant ERR-INVALID-SEVERITY     (err u103))
(define-constant ERR-INVALID-AMOUNT       (err u104))
(define-constant ERR-PRODUCT-NOT-FOUND    (err u105))
(define-constant ERR-INSUFFICIENT-TOKENS  (err u106))
(define-constant ERR-PROJECT-NOT-FOUND    (err u107))

;; Violation severity levels  1=low  2=medium  3=high  4=critical
(define-constant SEVERITY-LOW      u1)
(define-constant SEVERITY-MEDIUM   u2)
(define-constant SEVERITY-HIGH     u3)
(define-constant SEVERITY-CRITICAL u4)

;; Reputation score bounds
(define-constant MAX-REPUTATION  u1000)
(define-constant BASE-REPUTATION u500)

;; Minimum token stake to register as a stakeholder
(define-constant MIN-STAKE u100)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Global counters
(define-data-var next-product-id    uint u1)
(define-data-var next-violation-id  uint u1)
(define-data-var next-project-id    uint u1)

;; Stakeholder registry
;; Maps principal -> stakeholder record
(define-map stakeholders
  principal
  {
    name:            (string-ascii 64),
    staked-tokens:   uint,
    reputation:      uint,
    violation-count: uint,
    active:          bool
  }
)

;; Product registry
;; Maps product-id -> product record
(define-map products
  uint
  {
    owner:       principal,
    name:        (string-ascii 64),
    origin:      (string-ascii 64),
    created-at:  uint,
    active:      bool
  }
)

;; Violation log
;; Maps violation-id -> violation record
(define-map violations
  uint
  {
    product-id:  uint,
    reporter:    principal,
    offender:    principal,
    severity:    uint,
    description: (string-ascii 256),
    resolved:    bool,
    block-height: uint
  }
)

;; Sustainability / offset projects
;; Maps project-id -> project record
(define-map sustainability-projects
  uint
  {
    owner:        principal,
    description:  (string-ascii 256),
    impact-score: uint,
    verified:     bool
  }
)

;; Ethical debt: tracks how many unresolved violation-severity-points
;; each principal has accumulated
(define-map ethical-debt
  principal
  uint
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Clamp a uint value between lo and hi
(define-private (clamp-uint (value uint) (lo uint) (hi uint))
  (if (< value lo)
    lo
    (if (> value hi) hi value)
  )
)

;; Adjust reputation by delta (positive = increase, treated as signed via two calls)
(define-private (increase-reputation (who principal) (delta uint))
  (match (map-get? stakeholders who)
    entry
      (begin
        (map-set stakeholders who
          (merge entry {
            reputation: (clamp-uint (+ (get reputation entry) delta) u0 MAX-REPUTATION)
          })
        )
        (ok true)
      )
    ERR-NOT-REGISTERED
  )
)

(define-private (decrease-reputation (who principal) (delta uint))
  (match (map-get? stakeholders who)
    entry
      (begin
        (map-set stakeholders who
          (merge entry {
            reputation: (clamp-uint
                           (if (> delta (get reputation entry))
                             u0
                             (- (get reputation entry) delta))
                           u0
                           MAX-REPUTATION)
          })
        )
        (ok true)
      )
    ERR-NOT-REGISTERED
  )
)

;; Add to ethical-debt for a principal
(define-private (add-ethical-debt (who principal) (amount uint))
  (let ((current (default-to u0 (map-get? ethical-debt who))))
    (map-set ethical-debt who (+ current amount))
  )
)

;; Reduce ethical-debt for a principal (min 0)
(define-private (reduce-ethical-debt (who principal) (amount uint))
  (let ((current (default-to u0 (map-get? ethical-debt who))))
    (map-set ethical-debt who
      (if (> amount current) u0 (- current amount))
    )
  )
)

;; Reputation penalty scale by severity
(define-private (penalty-for-severity (severity uint))
  (if (is-eq severity SEVERITY-CRITICAL) u100
  (if (is-eq severity SEVERITY-HIGH)     u50
  (if (is-eq severity SEVERITY-MEDIUM)   u25
    u10                                  ;; low
  )))
)

;; ============================================================
;; PUBLIC FUNCTIONS
;; ============================================================

;; --- Stakeholder Management ---

;; Register as a stakeholder by staking tokens
;; The token commitment is tracked on-chain as a uint;
;; integration with a SIP-010 token is left for the calling layer.
(define-public (register-stakeholder
    (name (string-ascii 64))
    (stake-amount uint))
  (begin
    (asserts! (>= stake-amount MIN-STAKE) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? stakeholders tx-sender)) ERR-ALREADY-REGISTERED)
    (map-set stakeholders tx-sender
      {
        name:            name,
        staked-tokens:   stake-amount,
        reputation:      BASE-REPUTATION,
        violation-count: u0,
        active:          true
      }
    )
    (ok true)
  )
)

;; Update stake amount (add more tokens)
(define-public (increase-stake (additional uint))
  (begin
    (asserts! (> additional u0) ERR-INVALID-AMOUNT)
    (match (map-get? stakeholders tx-sender)
      entry
        (begin
          (map-set stakeholders tx-sender
            (merge entry { staked-tokens: (+ (get staked-tokens entry) additional) })
          )
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

;; --- Product Management ---

;; Register a new product
(define-public (register-product
    (name (string-ascii 64))
    (origin (string-ascii 64)))
  (let ((pid (var-get next-product-id)))
    (asserts! (is-some (map-get? stakeholders tx-sender)) ERR-NOT-REGISTERED)
    (map-set products pid
      {
        owner:      tx-sender,
        name:       name,
        origin:     origin,
        created-at: block-height,
        active:     true
      }
    )
    (var-set next-product-id (+ pid u1))
    (ok pid)
  )
)

;; Deactivate a product (owner only)
(define-public (deactivate-product (product-id uint))
  (match (map-get? products product-id)
    entry
      (begin
        (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
        (map-set products product-id (merge entry { active: false }))
        (ok true)
      )
    ERR-PRODUCT-NOT-FOUND
  )
)

;; --- Violation Reporting ---

;; Report a supply chain violation against an offending principal
(define-public (report-violation
    (product-id  uint)
    (offender    principal)
    (severity    uint)
    (description (string-ascii 256)))
  (let ((vid (var-get next-violation-id)))
    (asserts! (is-some (map-get? stakeholders tx-sender)) ERR-NOT-REGISTERED)
    (asserts! (is-some (map-get? stakeholders offender))  ERR-NOT-REGISTERED)
    (asserts! (is-some (map-get? products product-id))    ERR-PRODUCT-NOT-FOUND)
    (asserts!
      (or (is-eq severity SEVERITY-LOW)
      (or (is-eq severity SEVERITY-MEDIUM)
      (or (is-eq severity SEVERITY-HIGH)
          (is-eq severity SEVERITY-CRITICAL))))
      ERR-INVALID-SEVERITY)
    ;; Record violation
    (map-set violations vid
      {
        product-id:   product-id,
        reporter:     tx-sender,
        offender:     offender,
        severity:     severity,
        description:  description,
        resolved:     false,
        block-height: block-height
      }
    )
    (var-set next-violation-id (+ vid u1))
    ;; Apply reputation penalty to offender
    (try! (decrease-reputation offender (penalty-for-severity severity)))
    ;; Increment violation count for offender
    (match (map-get? stakeholders offender)
      offender-entry
        (map-set stakeholders offender
          (merge offender-entry
            { violation-count: (+ (get violation-count offender-entry) u1) }
          )
        )
      false
    )
    ;; Add ethical debt
    (add-ethical-debt offender severity)
    ;; Reward reporter for transparency
    (try! (increase-reputation tx-sender u5))
    (ok vid)
  )
)

;; Mark a violation as resolved (contract owner or reporter)
(define-public (resolve-violation (violation-id uint))
  (match (map-get? violations violation-id)
    entry
      (begin
        (asserts!
          (or (is-eq tx-sender CONTRACT-OWNER)
              (is-eq tx-sender (get reporter entry)))
          ERR-NOT-AUTHORIZED)
        (map-set violations violation-id (merge entry { resolved: true }))
        (ok true)
      )
    ERR-NOT-REGISTERED
  )
)

;; --- Sustainability Projects (Ethical Debt Offset) ---

;; Submit a sustainability project for verification
(define-public (submit-sustainability-project
    (description  (string-ascii 256))
    (impact-score uint))
  (let ((proj-id (var-get next-project-id)))
    (asserts! (is-some (map-get? stakeholders tx-sender)) ERR-NOT-REGISTERED)
    (asserts! (> impact-score u0) ERR-INVALID-AMOUNT)
    (map-set sustainability-projects proj-id
      {
        owner:        tx-sender,
        description:  description,
        impact-score: impact-score,
        verified:     false
      }
    )
    (var-set next-project-id (+ proj-id u1))
    (ok proj-id)
  )
)

;; Contract owner verifies a sustainability project and offsets ethical debt
(define-public (verify-and-offset-project (project-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (map-get? sustainability-projects project-id)
      proj
        (begin
          (asserts! (not (get verified proj)) ERR-NOT-AUTHORIZED)
          (map-set sustainability-projects project-id
            (merge proj { verified: true })
          )
          ;; Offset ethical debt by impact score
          (reduce-ethical-debt (get owner proj) (get impact-score proj))
          ;; Reward project owner
          (try! (increase-reputation (get owner proj) (/ (get impact-score proj) u10)))
          (ok true)
        )
      ERR-PROJECT-NOT-FOUND
    )
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

;; Get stakeholder info
(define-read-only (get-stakeholder (who principal))
  (map-get? stakeholders who)
)

;; Get product info
(define-read-only (get-product (product-id uint))
  (map-get? products product-id)
)

;; Get violation info
(define-read-only (get-violation (violation-id uint))
  (map-get? violations violation-id)
)

;; Get sustainability project info
(define-read-only (get-sustainability-project (project-id uint))
  (map-get? sustainability-projects project-id)
)

;; Get ethical debt for a principal
(define-read-only (get-ethical-debt (who principal))
  (default-to u0 (map-get? ethical-debt who))
)

;; Get current reputation score
(define-read-only (get-reputation (who principal))
  (match (map-get? stakeholders who)
    entry (some (get reputation entry))
    none
  )
)

;; Check if a stakeholder qualifies for reduced fees
;; Qualification: reputation >= 750 and zero ethical debt
(define-read-only (qualifies-for-reduced-fees (who principal))
  (match (map-get? stakeholders who)
    entry
      (and
        (>= (get reputation entry) u750)
        (is-eq (default-to u0 (map-get? ethical-debt who)) u0)
      )
    false
  )
)

;; Get the next IDs (useful for indexing)
(define-read-only (get-next-ids)
  {
    next-product-id:   (var-get next-product-id),
    next-violation-id: (var-get next-violation-id),
    next-project-id:   (var-get next-project-id)
  }
)
