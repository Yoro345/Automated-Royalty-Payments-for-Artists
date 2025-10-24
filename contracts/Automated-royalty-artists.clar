(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-percentage (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-advance-not-found (err u107))
(define-constant err-advance-already-approved (err u108))
(define-constant err-advance-not-approved (err u109))
(define-constant err-insufficient-collateral (err u110))
(define-constant err-escrow-not-found (err u111))
(define-constant err-milestone-not-found (err u112))
(define-constant err-escrow-expired (err u113))
(define-constant err-escrow-completed (err u114))
(define-constant err-milestone-already-completed (err u115))
(define-constant err-collaboration-not-found (err u116))
(define-constant err-collaboration-exists (err u117))
(define-constant err-invalid-split (err u118))
(define-constant err-not-collaborator (err u119))
(define-constant err-collaboration-locked (err u120))

(define-data-var next-artist-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var contract-fee-percentage uint u250)
(define-data-var next-advance-id uint u1)
(define-data-var next-escrow-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var next-collaboration-id uint u1)

(define-map artists
  { artist-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    royalty-percentage: uint,
    total-earned: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map artist-by-principal
  { owner: principal }
  { artist-id: uint }
)

(define-map revenue-streams
  { stream-id: uint }
  {
    name: (string-ascii 100),
    total-revenue: uint,
    distributed: uint,
    created-by: principal,
    created-at: uint,
    is-active: bool
  }
)

(define-map stream-artists
  { stream-id: uint, artist-id: uint }
  { percentage: uint }
)

(define-map payment-history
  { payment-id: uint }
  {
    artist-id: uint,
    stream-id: uint,
    amount: uint,
    timestamp: uint,
    tx-sender: principal
  }
)

(define-map advances
  { advance-id: uint }
  {
    artist-id: uint,
    amount: uint,
    repaid: uint,
    interest-rate: uint,
    due-date: uint,
    is-approved: bool,
    is-repaid: bool,
    lender: principal,
    created-at: uint
  }
)

(define-map escrows
  { escrow-id: uint }
  {
    client: principal,
    artist-id: uint,
    total-amount: uint,
    released-amount: uint,
    project-name: (string-ascii 100),
    expiry-block: uint,
    is-completed: bool,
    created-at: uint
  }
)

(define-map milestones
  { milestone-id: uint }
  {
    escrow-id: uint,
    description: (string-ascii 200),
    amount: uint,
    is-completed: bool,
    is-approved: bool,
    submission-block: uint,
    approval-block: uint
  }
)

(define-map collaborations
  { collaboration-id: uint }
  {
    name: (string-ascii 100),
    creator: principal,
    total-splits: uint,
    is-locked: bool,
    is-active: bool,
    created-at: uint
  }
)

(define-map collaboration-splits
  { collaboration-id: uint, artist-id: uint }
  {
    split-percentage: uint,
    is-approved: bool,
    approved-at: uint
  }
)

(define-map collaboration-revenue
  { collaboration-id: uint }
  {
    total-collected: uint,
    total-distributed: uint,
    last-distribution: uint
  }
)

(define-map artist-collaborations
  { artist-id: uint, collaboration-id: uint }
  { is-member: bool }
)

(define-data-var next-stream-id uint u1)

(define-read-only (get-artist (artist-id uint))
  (map-get? artists { artist-id: artist-id })
)

(define-read-only (get-artist-by-principal (owner principal))
  (match (map-get? artist-by-principal { owner: owner })
    entry (get-artist (get artist-id entry))
    none
  )
)

(define-read-only (get-revenue-stream (stream-id uint))
  (map-get? revenue-streams { stream-id: stream-id })
)

(define-read-only (get-stream-artist-percentage (stream-id uint) (artist-id uint))
  (map-get? stream-artists { stream-id: stream-id, artist-id: artist-id })
)

(define-read-only (get-payment (payment-id uint))
  (map-get? payment-history { payment-id: payment-id })
)

(define-read-only (get-contract-fee-percentage)
  (var-get contract-fee-percentage)
)

(define-read-only (calculate-artist-payment (stream-id uint) (artist-id uint) (revenue-amount uint))
  (match (get-stream-artist-percentage stream-id artist-id)
    percentage-entry
    (let
      (
        (artist-percentage (get percentage percentage-entry))
        (contract-fee (/ (* revenue-amount (var-get contract-fee-percentage)) u10000))
        (net-revenue (- revenue-amount contract-fee))
        (artist-payment (/ (* net-revenue artist-percentage) u10000))
      )
      (ok artist-payment)
    )
    err-not-found
  )
)

(define-public (register-artist (name (string-ascii 50)) (royalty-percentage uint))
  (let
    (
      (artist-id (var-get next-artist-id))
      (current-block stacks-block-height)
    )
    (asserts! (<= royalty-percentage u10000) err-invalid-percentage)
    (asserts! (is-none (map-get? artist-by-principal { owner: tx-sender })) err-already-exists)
    
    (map-set artists
      { artist-id: artist-id }
      {
        owner: tx-sender,
        name: name,
        royalty-percentage: royalty-percentage,
        total-earned: u0,
        is-active: true,
        created-at: current-block
      }
    )
    
    (map-set artist-by-principal
      { owner: tx-sender }
      { artist-id: artist-id }
    )
    
    (var-set next-artist-id (+ artist-id u1))
    (ok artist-id)
  )
)

(define-public (create-revenue-stream (name (string-ascii 100)))
  (let
    (
      (stream-id (var-get next-stream-id))
      (current-block stacks-block-height)
    )
    (map-set revenue-streams
      { stream-id: stream-id }
      {
        name: name,
        total-revenue: u0,
        distributed: u0,
        created-by: tx-sender,
        created-at: current-block,
        is-active: true
      }
    )
    
    (var-set next-stream-id (+ stream-id u1))
    (ok stream-id)
  )
)

(define-public (add-artist-to-stream (stream-id uint) (artist-id uint) (percentage uint))
  (let
    (
      (stream (unwrap! (get-revenue-stream stream-id) err-not-found))
      (artist (unwrap! (get-artist artist-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get created-by stream)) err-unauthorized)
    (asserts! (<= percentage u10000) err-invalid-percentage)
    (asserts! (get is-active stream) err-not-found)
    (asserts! (get is-active artist) err-not-found)
    
    (map-set stream-artists
      { stream-id: stream-id, artist-id: artist-id }
      { percentage: percentage }
    )
    
    (ok true)
  )
)

(define-public (add-revenue (stream-id uint))
  (let
    (
      (stream (unwrap! (get-revenue-stream stream-id) err-not-found))
      (revenue-amount (stx-get-balance tx-sender))
    )
    (asserts! (> revenue-amount u0) err-invalid-amount)
    (asserts! (get is-active stream) err-not-found)
    
    (try! (stx-transfer? revenue-amount tx-sender (as-contract tx-sender)))
    
    (map-set revenue-streams
      { stream-id: stream-id }
      (merge stream { total-revenue: (+ (get total-revenue stream) revenue-amount) })
    )
    
    (ok revenue-amount)
  )
)

(define-public (distribute-royalties (stream-id uint) (artist-id uint))
  (let
    (
      (stream (unwrap! (get-revenue-stream stream-id) err-not-found))
      (artist (unwrap! (get-artist artist-id) err-not-found))
      (undistributed (- (get total-revenue stream) (get distributed stream)))
      (payment-amount (unwrap! (calculate-artist-payment stream-id artist-id undistributed) err-not-found))
      (payment-id (var-get next-payment-id))
      (current-block stacks-block-height)
    )
    (asserts! (> payment-amount u0) err-invalid-amount)
    (asserts! (get is-active stream) err-not-found)
    (asserts! (get is-active artist) err-not-found)
    
    (try! (as-contract (stx-transfer? payment-amount tx-sender (get owner artist))))
    
    (map-set revenue-streams
      { stream-id: stream-id }
      (merge stream { distributed: (+ (get distributed stream) payment-amount) })
    )
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist { total-earned: (+ (get total-earned artist) payment-amount) })
    )
    
    (map-set payment-history
      { payment-id: payment-id }
      {
        artist-id: artist-id,
        stream-id: stream-id,
        amount: payment-amount,
        timestamp: current-block,
        tx-sender: tx-sender
      }
    )
    
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-amount)
  )
)

(define-private (process-single-payment (artist-id uint) (stream-id uint) (undistributed uint) (current-block uint))
  (match (get-artist artist-id)
    artist
    (match (get-stream-artist-percentage stream-id artist-id)
      percentage-entry
      (let
        (
          (payment-amount (unwrap! (calculate-artist-payment stream-id artist-id undistributed) err-not-found))
          (payment-id (var-get next-payment-id))
        )
        (if (and (> payment-amount u0) (get is-active artist))
          (begin
            (try! (as-contract (stx-transfer? payment-amount tx-sender (get owner artist))))
            (map-set artists
              { artist-id: artist-id }
              (merge artist { total-earned: (+ (get total-earned artist) payment-amount) })
            )
            (map-set payment-history
              { payment-id: payment-id }
              {
                artist-id: artist-id,
                stream-id: stream-id,
                amount: payment-amount,
                timestamp: current-block,
                tx-sender: tx-sender
              }
            )
            (var-set next-payment-id (+ payment-id u1))
            (ok payment-amount)
          )
          (ok u0)
        )
      )
      (ok u0)
    )
    (ok u0)
  )
)

(define-public (distribute-batch-royalties (stream-id uint) (artist-ids (list 20 uint)))
  (let
    (
      (stream (unwrap! (get-revenue-stream stream-id) err-not-found))
      (undistributed (- (get total-revenue stream) (get distributed stream)))
      (current-block stacks-block-height)
      (final-state (fold process-batch-fold artist-ids { stream-id: stream-id, undistributed: undistributed, total-paid: u0, current-block: current-block }))
      (total-distributed (get total-paid final-state))
    )
    (asserts! (> undistributed u0) err-invalid-amount)
    (asserts! (get is-active stream) err-not-found)
    (asserts! (is-eq tx-sender (get created-by stream)) err-unauthorized)
    
    (map-set revenue-streams
      { stream-id: stream-id }
      (merge stream { distributed: (+ (get distributed stream) total-distributed) })
    )
    (ok total-distributed)
  )
)

(define-private (process-batch-fold (artist-id uint) (state { stream-id: uint, undistributed: uint, total-paid: uint, current-block: uint }))
  (let
    (
      (payment-result (process-single-payment artist-id (get stream-id state) (get undistributed state) (get current-block state)))
      (payment-amount (if (is-ok payment-result) (unwrap-panic payment-result) u0))
    )
    (merge state { total-paid: (+ (get total-paid state) payment-amount) })
  )
)

(define-public (update-artist-status (artist-id uint) (is-active bool))
  (let
    (
      (artist (unwrap! (get-artist artist-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner artist)) err-unauthorized)
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist { is-active: is-active })
    )
    
    (ok true)
  )
)

(define-public (update-stream-status (stream-id uint) (is-active bool))
  (let
    (
      (stream (unwrap! (get-revenue-stream stream-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get created-by stream)) err-unauthorized)
    
    (map-set revenue-streams
      { stream-id: stream-id }
      (merge stream { is-active: is-active })
    )
    
    (ok true)
  )
)

(define-public (set-contract-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-percentage)
    (var-set contract-fee-percentage new-fee)
    (ok new-fee)
  )
)

(define-public (emergency-withdraw)
  (let
    (
      (contract-balance (stx-get-balance (as-contract tx-sender)))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> contract-balance u0) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? contract-balance tx-sender contract-owner)))
    (ok contract-balance)
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-next-artist-id)
  (var-get next-artist-id)
)

(define-read-only (get-next-stream-id)
  (var-get next-stream-id)
)

(define-read-only (get-next-payment-id)
  (var-get next-payment-id)
)

(define-read-only (get-advance (advance-id uint))
  (map-get? advances { advance-id: advance-id })
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)

(define-read-only (get-next-milestone-id)
  (var-get next-milestone-id)
)

(define-read-only (calculate-advance-total (advance-id uint))
  (match (get-advance advance-id)
    advance
    (let
      (
        (principal-amount (get amount advance))
        (interest-amount (/ (* principal-amount (get interest-rate advance)) u10000))
      )
      (ok (+ principal-amount interest-amount))
    )
    err-advance-not-found
  )
)

(define-public (request-advance (amount uint) (interest-rate uint) (duration-blocks uint))
  (let
    (
      (artist-lookup (unwrap! (map-get? artist-by-principal { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-lookup))
      (artist-info (unwrap! (get-artist artist-id) err-not-found))
      (advance-id (var-get next-advance-id))
      (current-block stacks-block-height)
      (due-date (+ current-block duration-blocks))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= interest-rate u5000) err-invalid-percentage)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    (asserts! (get is-active artist-info) err-unauthorized)
    
    (map-set advances
      { advance-id: advance-id }
      {
        artist-id: artist-id,
        amount: amount,
        repaid: u0,
        interest-rate: interest-rate,
        due-date: due-date,
        is-approved: false,
        is-repaid: false,
        lender: tx-sender,
        created-at: current-block
      }
    )
    
    (var-set next-advance-id (+ advance-id u1))
    (ok advance-id)
  )
)

(define-public (approve-advance (advance-id uint))
  (let
    (
      (advance (unwrap! (get-advance advance-id) err-advance-not-found))
      (artist (unwrap! (get-artist (get artist-id advance)) err-not-found))
    )
    (asserts! (not (get is-approved advance)) err-advance-already-approved)
    (asserts! (>= (stx-get-balance tx-sender) (get amount advance)) err-insufficient-funds)
    (asserts! (get is-active artist) err-unauthorized)
    
    (try! (stx-transfer? (get amount advance) tx-sender (get owner artist)))
    
    (map-set advances
      { advance-id: advance-id }
      (merge advance { is-approved: true, lender: tx-sender })
    )
    
    (ok true)
  )
)

(define-public (repay-advance (advance-id uint) (amount uint))
  (let
    (
      (advance (unwrap! (get-advance advance-id) err-advance-not-found))
      (artist (unwrap! (get-artist (get artist-id advance)) err-not-found))
      (total-owed (unwrap! (calculate-advance-total advance-id) err-advance-not-found))
      (already-repaid (get repaid advance))
      (remaining-debt (- total-owed already-repaid))
      (payment-amount (if (<= amount remaining-debt) amount remaining-debt))
      (new-repaid (+ already-repaid payment-amount))
      (is-fully-repaid (>= new-repaid total-owed))
    )
    (asserts! (get is-approved advance) err-advance-not-approved)
    (asserts! (not (get is-repaid advance)) err-advance-not-found)
    (asserts! (is-eq tx-sender (get owner artist)) err-unauthorized)
    (asserts! (> payment-amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) err-insufficient-funds)
    
    (try! (stx-transfer? payment-amount tx-sender (get lender advance)))
    
    (map-set advances
      { advance-id: advance-id }
      (merge advance { repaid: new-repaid, is-repaid: is-fully-repaid })
    )
    
    (ok payment-amount)
  )
)

(define-public (create-escrow (artist-id uint) (total-amount uint) (project-name (string-ascii 100)) (duration-blocks uint))
  (let
    (
      (artist (unwrap! (get-artist artist-id) err-not-found))
      (escrow-id (var-get next-escrow-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block duration-blocks))
    )
    (asserts! (> total-amount u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    (asserts! (get is-active artist) err-unauthorized)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) err-insufficient-funds)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set escrows
      { escrow-id: escrow-id }
      {
        client: tx-sender,
        artist-id: artist-id,
        total-amount: total-amount,
        released-amount: u0,
        project-name: project-name,
        expiry-block: expiry-block,
        is-completed: false,
        created-at: current-block
      }
    )
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (create-milestone (escrow-id uint) (description (string-ascii 200)) (amount uint))
  (let
    (
      (escrow (unwrap! (get-escrow escrow-id) err-escrow-not-found))
      (milestone-id (var-get next-milestone-id))
    )
    (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
    (asserts! (not (get is-completed escrow)) err-escrow-completed)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= (+ (get released-amount escrow) amount) (get total-amount escrow)) err-invalid-amount)
    
    (map-set milestones
      { milestone-id: milestone-id }
      {
        escrow-id: escrow-id,
        description: description,
        amount: amount,
        is-completed: false,
        is-approved: false,
        submission-block: u0,
        approval-block: u0
      }
    )
    
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (submit-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (get-milestone milestone-id) err-milestone-not-found))
      (escrow (unwrap! (get-escrow (get escrow-id milestone)) err-escrow-not-found))
      (artist (unwrap! (get-artist (get artist-id escrow)) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner artist)) err-unauthorized)
    (asserts! (not (get is-completed milestone)) err-milestone-already-completed)
    (asserts! (not (get is-completed escrow)) err-escrow-completed)
    (asserts! (< current-block (get expiry-block escrow)) err-escrow-expired)
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone { is-completed: true, submission-block: current-block })
    )
    
    (ok true)
  )
)

(define-public (approve-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (get-milestone milestone-id) err-milestone-not-found))
      (escrow (unwrap! (get-escrow (get escrow-id milestone)) err-escrow-not-found))
      (artist (unwrap! (get-artist (get artist-id escrow)) err-not-found))
      (current-block stacks-block-height)
      (payment-amount (get amount milestone))
      (contract-fee (/ (* payment-amount (var-get contract-fee-percentage)) u10000))
      (artist-payment (- payment-amount contract-fee))
    )
    (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
    (asserts! (get is-completed milestone) err-not-found)
    (asserts! (not (get is-approved milestone)) err-milestone-already-completed)
    (asserts! (not (get is-completed escrow)) err-escrow-completed)
    (asserts! (<= (+ (get released-amount escrow) payment-amount) (get total-amount escrow)) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? artist-payment tx-sender (get owner artist))))
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone { is-approved: true, approval-block: current-block })
    )
    
    (map-set escrows
      { escrow-id: (get escrow-id milestone) }
      (merge escrow { 
        released-amount: (+ (get released-amount escrow) payment-amount),
        is-completed: (>= (+ (get released-amount escrow) payment-amount) (get total-amount escrow))
      })
    )
    
    (map-set artists
      { artist-id: (get artist-id escrow) }
      (merge artist { total-earned: (+ (get total-earned artist) artist-payment) })
    )
    
    (ok artist-payment)
  )
)

(define-public (refund-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (get-escrow escrow-id) err-escrow-not-found))
      (current-block stacks-block-height)
      (refund-amount (- (get total-amount escrow) (get released-amount escrow)))
    )
    (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
    (asserts! (>= current-block (get expiry-block escrow)) err-invalid-amount)
    (asserts! (not (get is-completed escrow)) err-escrow-completed)
    (asserts! (> refund-amount u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender (get client escrow))))
    
    (map-set escrows
      { escrow-id: escrow-id }
      (merge escrow { is-completed: true })
    )
    
    (ok refund-amount)
  )
)

(define-public (create-collaboration (name (string-ascii 100)))
  (let
    (
      (collaboration-id (var-get next-collaboration-id))
      (current-block stacks-block-height)
    )
    (map-set collaborations
      { collaboration-id: collaboration-id }
      {
        name: name,
        creator: tx-sender,
        total-splits: u0,
        is-locked: false,
        is-active: true,
        created-at: current-block
      }
    )
    
    (map-set collaboration-revenue
      { collaboration-id: collaboration-id }
      {
        total-collected: u0,
        total-distributed: u0,
        last-distribution: u0
      }
    )
    
    (var-set next-collaboration-id (+ collaboration-id u1))
    (ok collaboration-id)
  )
)

(define-public (add-collaborator (collaboration-id uint) (artist-id uint) (split-percentage uint))
  (let
    (
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
      (artist (unwrap! (get-artist artist-id) err-not-found))
      (existing-split (map-get? collaboration-splits { collaboration-id: collaboration-id, artist-id: artist-id }))
      (current-splits (get total-splits collab))
      (new-total-splits (+ current-splits split-percentage))
    )
    (asserts! (is-eq tx-sender (get creator collab)) err-unauthorized)
    (asserts! (not (get is-locked collab)) err-collaboration-locked)
    (asserts! (get is-active collab) err-not-found)
    (asserts! (get is-active artist) err-not-found)
    (asserts! (is-none existing-split) err-collaboration-exists)
    (asserts! (<= new-total-splits u10000) err-invalid-split)
    (asserts! (> split-percentage u0) err-invalid-percentage)
    
    (map-set collaboration-splits
      { collaboration-id: collaboration-id, artist-id: artist-id }
      {
        split-percentage: split-percentage,
        is-approved: false,
        approved-at: u0
      }
    )
    
    (map-set artist-collaborations
      { artist-id: artist-id, collaboration-id: collaboration-id }
      { is-member: true }
    )
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collab { total-splits: new-total-splits })
    )
    
    (ok true)
  )
)

(define-public (approve-collaboration-split (collaboration-id uint))
  (let
    (
      (artist-lookup (unwrap! (map-get? artist-by-principal { owner: tx-sender }) err-not-found))
      (artist-id (get artist-id artist-lookup))
      (split (unwrap! (map-get? collaboration-splits { collaboration-id: collaboration-id, artist-id: artist-id }) err-not-collaborator))
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (not (get is-approved split)) err-collaboration-exists)
    (asserts! (not (get is-locked collab)) err-collaboration-locked)
    (asserts! (get is-active collab) err-not-found)
    
    (map-set collaboration-splits
      { collaboration-id: collaboration-id, artist-id: artist-id }
      (merge split { is-approved: true, approved-at: current-block })
    )
    
    (ok true)
  )
)

(define-public (lock-collaboration (collaboration-id uint))
  (let
    (
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
    )
    (asserts! (is-eq tx-sender (get creator collab)) err-unauthorized)
    (asserts! (not (get is-locked collab)) err-collaboration-locked)
    (asserts! (is-eq (get total-splits collab) u10000) err-invalid-split)
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collab { is-locked: true })
    )
    
    (ok true)
  )
)

(define-public (add-collaboration-revenue (collaboration-id uint) (amount uint))
  (let
    (
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
      (revenue (unwrap! (map-get? collaboration-revenue { collaboration-id: collaboration-id }) err-not-found))
    )
    (asserts! (get is-locked collab) err-collaboration-locked)
    (asserts! (get is-active collab) err-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set collaboration-revenue
      { collaboration-id: collaboration-id }
      (merge revenue { total-collected: (+ (get total-collected revenue) amount) })
    )
    
    (ok amount)
  )
)

(define-public (distribute-collaboration-payment (collaboration-id uint) (artist-id uint))
  (let
    (
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
      (artist (unwrap! (get-artist artist-id) err-not-found))
      (split (unwrap! (map-get? collaboration-splits { collaboration-id: collaboration-id, artist-id: artist-id }) err-not-collaborator))
      (revenue (unwrap! (map-get? collaboration-revenue { collaboration-id: collaboration-id }) err-not-found))
      (undistributed (- (get total-collected revenue) (get total-distributed revenue)))
      (artist-share (/ (* undistributed (get split-percentage split)) u10000))
      (contract-fee (/ (* artist-share (var-get contract-fee-percentage)) u10000))
      (final-payment (- artist-share contract-fee))
      (current-block stacks-block-height)
    )
    (asserts! (get is-locked collab) err-collaboration-locked)
    (asserts! (get is-active collab) err-not-found)
    (asserts! (get is-approved split) err-not-collaborator)
    (asserts! (get is-active artist) err-not-found)
    (asserts! (> final-payment u0) err-invalid-amount)
    (asserts! (> undistributed u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? final-payment tx-sender (get owner artist))))
    
    (map-set collaboration-revenue
      { collaboration-id: collaboration-id }
      (merge revenue { 
        total-distributed: (+ (get total-distributed revenue) artist-share),
        last-distribution: current-block
      })
    )
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist { total-earned: (+ (get total-earned artist) final-payment) })
    )
    
    (ok final-payment)
  )
)

(define-public (distribute-all-collaboration-payments (collaboration-id uint) (artist-ids (list 10 uint)))
  (let
    (
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
      (revenue (unwrap! (map-get? collaboration-revenue { collaboration-id: collaboration-id }) err-not-found))
      (undistributed (- (get total-collected revenue) (get total-distributed revenue)))
      (current-block stacks-block-height)
      (final-state (fold process-collab-payment artist-ids { collaboration-id: collaboration-id, total-paid: u0, undistributed: undistributed, current-block: current-block }))
      (total-distributed (get total-paid final-state))
    )
    (asserts! (get is-locked collab) err-collaboration-locked)
    (asserts! (get is-active collab) err-not-found)
    (asserts! (> undistributed u0) err-invalid-amount)
    
    (map-set collaboration-revenue
      { collaboration-id: collaboration-id }
      (merge revenue { 
        total-distributed: (+ (get total-distributed revenue) total-distributed),
        last-distribution: current-block
      })
    )
    
    (ok total-distributed)
  )
)

(define-private (process-collab-payment (artist-id uint) (state { collaboration-id: uint, total-paid: uint, undistributed: uint, current-block: uint }))
  (let
    (
      (collaboration-id (get collaboration-id state))
      (artist-result (get-artist artist-id))
      (split-result (map-get? collaboration-splits { collaboration-id: collaboration-id, artist-id: artist-id }))
    )
    (match artist-result
      artist
      (match split-result
        split
        (if (and (get is-approved split) (get is-active artist))
          (let
            (
              (artist-share (/ (* (get undistributed state) (get split-percentage split)) u10000))
              (contract-fee (/ (* artist-share (var-get contract-fee-percentage)) u10000))
              (final-payment (- artist-share contract-fee))
            )
            (if (> final-payment u0)
              (match (as-contract (stx-transfer? final-payment tx-sender (get owner artist)))
                success
                (begin
                  (map-set artists
                    { artist-id: artist-id }
                    (merge artist { total-earned: (+ (get total-earned artist) final-payment) })
                  )
                  (merge state { total-paid: (+ (get total-paid state) artist-share) })
                )
                error state
              )
              state
            )
          )
          state
        )
        state
      )
      state
    )
  )
)

(define-public (update-collaboration-status (collaboration-id uint) (is-active bool))
  (let
    (
      (collab (unwrap! (map-get? collaborations { collaboration-id: collaboration-id }) err-collaboration-not-found))
    )
    (asserts! (is-eq tx-sender (get creator collab)) err-unauthorized)
    
    (map-set collaborations
      { collaboration-id: collaboration-id }
      (merge collab { is-active: is-active })
    )
    
    (ok true)
  )
)

(define-read-only (get-collaboration (collaboration-id uint))
  (map-get? collaborations { collaboration-id: collaboration-id })
)

(define-read-only (get-collaboration-split (collaboration-id uint) (artist-id uint))
  (map-get? collaboration-splits { collaboration-id: collaboration-id, artist-id: artist-id })
)

(define-read-only (get-collaboration-revenue (collaboration-id uint))
  (map-get? collaboration-revenue { collaboration-id: collaboration-id })
)

(define-read-only (is-collaboration-member (artist-id uint) (collaboration-id uint))
  (match (map-get? artist-collaborations { artist-id: artist-id, collaboration-id: collaboration-id })
    entry (get is-member entry)
    false
  )
)

(define-read-only (get-next-collaboration-id)
  (var-get next-collaboration-id)
)

(define-read-only (calculate-collaboration-share (collaboration-id uint) (artist-id uint))
  (match (map-get? collaboration-revenue { collaboration-id: collaboration-id })
    revenue
    (match (map-get? collaboration-splits { collaboration-id: collaboration-id, artist-id: artist-id })
      split
      (let
        (
          (undistributed (- (get total-collected revenue) (get total-distributed revenue)))
          (artist-share (/ (* undistributed (get split-percentage split)) u10000))
          (contract-fee (/ (* artist-share (var-get contract-fee-percentage)) u10000))
          (final-payment (- artist-share contract-fee))
        )
        (ok {
          gross-share: artist-share,
          contract-fee: contract-fee,
          net-payment: final-payment,
          split-percentage: (get split-percentage split)
        })
      )
      err-not-collaborator
    )
    err-collaboration-not-found
  )
)

