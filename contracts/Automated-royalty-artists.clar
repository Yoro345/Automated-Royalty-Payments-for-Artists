(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-percentage (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-invalid-amount (err u106))

(define-data-var next-artist-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var contract-fee-percentage uint u250)

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

