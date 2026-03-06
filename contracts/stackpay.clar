;; StackPay — Payroll & Salary Streaming

(define-map streams
  { id: uint }
  {
    employer: principal,
    employee: principal,
    rate-per-block: uint,
    last-withdraw-block: uint,
    balance: uint,
    active: bool
  }
)

(define-data-var stream-id-counter uint u0)

;; Create a new salary stream
(define-public (create-stream (employee principal) (rate-per-block uint) (fund uint))
  (let ((id (+ (var-get stream-id-counter) u1)))
    ;; Transfer STX from employer to contract
    (try! (stx-transfer? fund tx-sender (as-contract tx-sender)))
    ;; Store stream data
    (map-set streams
      { id: id }
      {
        employer: tx-sender,
        employee: employee,
        rate-per-block: rate-per-block,
        last-withdraw-block: block-height,
        balance: fund,
        active: true
      }
    )
    ;; Increment stream ID counter
    (var-set stream-id-counter id)
    (ok id)
  )
)

;; Withdraw accrued salary for a stream
(define-public (withdraw (id uint))
  (let ((s (map-get? streams { id: id })))
    (match s stream
      (begin
        (asserts! (is-eq tx-sender (get employee stream)) (err u1))
        (asserts! (get active stream) (err u6)) ;; Only active streams can withdraw
        (let (
          (blocks (- block-height (get last-withdraw-block stream)))
          (amount (* blocks (get rate-per-block stream)))
          (payable (min amount (get balance stream)))
        )
          ;; Transfer accrued STX to employee
          (try! (stx-transfer? payable (as-contract tx-sender) tx-sender))
          ;; Update stream
          (map-set streams
            { id: id }
            (merge stream {
              last-withdraw-block: block-height,
              balance: (- (get balance stream) payable)
            })
          )
          (ok payable)
        )
      )
      (err u2)
    )
  )
)

;; Cancel a stream (employer only)
(define-public (cancel-stream (id uint))
  (let ((s (map-get? streams { id: id })))
    (match s stream
      (begin
        (asserts! (is-eq tx-sender (get employer stream)) (err u3))
        ;; Refund remaining balance to employer
        (try! (stx-transfer? (get balance stream) (as-contract tx-sender) (get employer stream)))
        ;; Mark stream inactive
        (map-set streams
          { id: id }
          (merge stream { balance: u0, active: false })
        )
        (ok true)
      )
      (err u4)
    )
  )
)

;; Pause a stream (employer only)
(define-public (pause-stream (id uint))
  (let ((s (map-get? streams { id: id })))
    (match s stream
      (begin
        (asserts! (is-eq tx-sender (get employer stream)) (err u7))
        ;; Mark stream as inactive without refunding
        (map-set streams
          { id: id }
          (merge stream { active: false })
        )
        (ok true)
      )
      (err u8)
    )
  )
)

;; Resume a paused stream (employer only)
(define-public (resume-stream (id uint))
  (let ((s (map-get? streams { id: id })))
    (match s stream
      (begin
        (asserts! (is-eq tx-sender (get employer stream)) (err u9))
        ;; Reactivate stream and reset last-withdraw-block
        (map-set streams
          { id: id }
          (merge stream { active: true, last-withdraw-block: block-height })
        )
        (ok true)
      )
      (err u10)
    )
  )
)

;; Update rate of a stream (employer only)
(define-public (update-rate (id uint) (new-rate uint))
  (let ((s (map-get? streams { id: id })))
    (match s stream
      (begin
        (asserts! (is-eq tx-sender (get employer stream)) (err u11))
        ;; Update rate
        (map-set streams
          { id: id }
          (merge stream { rate-per-block: new-rate })
        )
        (ok true)
      )
      (err u12)
    )
  )
)

;; Read-only function to fetch all active streams
(define-read-only (get-all-streams)
  (begin
    (define streams-list (list))
    (define counter (var-get stream-id-counter))

    (define (loop i acc)
      (if (> i counter)
          acc
          (let ((s (map-get? streams { id: i })))
            (if (and s (get active s))
                (loop (+ i u1) (cons s acc))
                (loop (+ i u1) acc)
            )
          )
      )
    )

    (loop u1 streams-list)
  )
)
