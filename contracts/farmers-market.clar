;; Farmers Market Management Platform
;; A comprehensive smart contract for managing local farmers markets
;; Features: Vendor registration, space allocation, payment processing, customer engagement

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_VENDOR_NOT_FOUND (err u1002))
(define-constant ERR_SPACE_NOT_AVAILABLE (err u1003))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u1004))
(define-constant ERR_INVALID_SPACE_ID (err u1005))
(define-constant ERR_VENDOR_ALREADY_EXISTS (err u1006))
(define-constant ERR_MARKET_NOT_ACTIVE (err u1007))
(define-constant ERR_ALREADY_RESERVED (err u1008))

;; Data Variables
(define-data-var market-active bool true)
(define-data-var space-rental-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var next-vendor-id uint u1)
(define-data-var next-space-id uint u1)

;; Data Maps
(define-map vendors
    { vendor-id: uint }
    {
        owner: principal,
        name: (string-ascii 50),
        description: (string-ascii 200),
        contact-info: (string-ascii 100),
        is-verified: bool,
        registration-date: uint,
        total-sales: uint
    }
)

(define-map market-spaces
    { space-id: uint }
    {
        size: (string-ascii 20),
        location: (string-ascii 50),
        rental-fee: uint,
        is-occupied: bool,
        rented-by: (optional uint),
        rental-start: (optional uint),
        rental-end: (optional uint)
    }
)

(define-map vendor-spaces
    { vendor-id: uint }
    { space-ids: (list 5 uint) }
)

(define-map customer-reviews
    { reviewer: principal, vendor-id: uint }
    {
        rating: uint,
        comment: (string-ascii 200),
        review-date: uint
    }
)

(define-map vendor-ratings
    { vendor-id: uint }
    {
        total-rating: uint,
        review-count: uint,
        average-rating: uint
    }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (calculate-average-rating (total-rating uint) (review-count uint))
    (if (> review-count u0)
        (/ (* total-rating u100) review-count)
        u0
    )
)

;; Public Functions

;; Market Management
(define-public (set-market-status (active bool))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set market-active active)
        (ok true)
    )
)

(define-public (update-space-rental-fee (new-fee uint))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set space-rental-fee new-fee)
        (ok true)
    )
)

;; Vendor Registration
(define-public (register-vendor (name (string-ascii 50)) (description (string-ascii 200)) (contact-info (string-ascii 100)))
    (let (
        (vendor-id (var-get next-vendor-id))
    )
        (asserts! (var-get market-active) ERR_MARKET_NOT_ACTIVE)
        (asserts! (is-none (map-get? vendors { vendor-id: vendor-id })) ERR_VENDOR_ALREADY_EXISTS)
        
        (map-set vendors
            { vendor-id: vendor-id }
            {
                owner: tx-sender,
                name: name,
                description: description,
                contact-info: contact-info,
                is-verified: false,
                registration-date: stacks-block-height,
                total-sales: u0
            }
        )
        
        (var-set next-vendor-id (+ vendor-id u1))
        (ok vendor-id)
    )
)

(define-public (verify-vendor (vendor-id uint))
    (let (
        (vendor (unwrap! (map-get? vendors { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
    )
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        
        (map-set vendors
            { vendor-id: vendor-id }
            (merge vendor { is-verified: true })
        )
        (ok true)
    )
)

;; Space Management
(define-public (create-market-space (size (string-ascii 20)) (location (string-ascii 50)) (rental-fee uint))
    (let (
        (space-id (var-get next-space-id))
    )
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        
        (map-set market-spaces
            { space-id: space-id }
            {
                size: size,
                location: location,
                rental-fee: rental-fee,
                is-occupied: false,
                rented-by: none,
                rental-start: none,
                rental-end: none
            }
        )
        
        (var-set next-space-id (+ space-id u1))
        (ok space-id)
    )
)

(define-public (rent-space (vendor-id uint) (space-id uint) (rental-duration uint))
    (let (
        (vendor (unwrap! (map-get? vendors { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
        (space (unwrap! (map-get? market-spaces { space-id: space-id }) ERR_INVALID_SPACE_ID))
        (rental-fee (get rental-fee space))
        (current-spaces (default-to { space-ids: (list) } (map-get? vendor-spaces { vendor-id: vendor-id })))
    )
        (asserts! (var-get market-active) ERR_MARKET_NOT_ACTIVE)
        (asserts! (is-eq (get owner vendor) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (not (get is-occupied space)) ERR_SPACE_NOT_AVAILABLE)
        (asserts! (>= (stx-get-balance tx-sender) rental-fee) ERR_INSUFFICIENT_PAYMENT)
        
        ;; Transfer payment to contract owner
        (try! (stx-transfer? rental-fee tx-sender CONTRACT_OWNER))
        
        ;; Update space status
        (map-set market-spaces
            { space-id: space-id }
            (merge space {
                is-occupied: true,
                rented-by: (some vendor-id),
                rental-start: (some stacks-block-height),
                rental-end: (some (+ stacks-block-height rental-duration))
            })
        )
        
        ;; Update vendor's spaces
        (map-set vendor-spaces
            { vendor-id: vendor-id }
            { space-ids: (unwrap! (as-max-len? (append (get space-ids current-spaces) space-id) u5) ERR_ALREADY_RESERVED) }
        )
        
        (ok true)
    )
)

;; Customer Engagement
(define-public (leave-review (vendor-id uint) (rating uint) (comment (string-ascii 200)))
    (let (
        (vendor (unwrap! (map-get? vendors { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
        (existing-rating (default-to { total-rating: u0, review-count: u0, average-rating: u0 } 
                                    (map-get? vendor-ratings { vendor-id: vendor-id })))
        (new-total-rating (+ (get total-rating existing-rating) rating))
        (new-review-count (+ (get review-count existing-rating) u1))
        (new-average-rating (calculate-average-rating new-total-rating new-review-count))
    )
        (asserts! (var-get market-active) ERR_MARKET_NOT_ACTIVE)
        (asserts! (and (>= rating u1) (<= rating u5)) (err u1009)) ;; Rating must be 1-5
        
        ;; Store individual review
        (map-set customer-reviews
            { reviewer: tx-sender, vendor-id: vendor-id }
            {
                rating: rating,
                comment: comment,
                review-date: stacks-block-height
            }
        )
        
        ;; Update vendor rating summary
        (map-set vendor-ratings
            { vendor-id: vendor-id }
            {
                total-rating: new-total-rating,
                review-count: new-review-count,
                average-rating: new-average-rating
            }
        )
        
        (ok true)
    )
)

(define-public (record-sale (vendor-id uint) (sale-amount uint))
    (let (
        (vendor (unwrap! (map-get? vendors { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
    )
        (asserts! (is-eq (get owner vendor) tx-sender) ERR_NOT_AUTHORIZED)
        
        (map-set vendors
            { vendor-id: vendor-id }
            (merge vendor { total-sales: (+ (get total-sales vendor) sale-amount) })
        )
        
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-vendor (vendor-id uint))
    (map-get? vendors { vendor-id: vendor-id })
)

(define-read-only (get-market-space (space-id uint))
    (map-get? market-spaces { space-id: space-id })
)

(define-read-only (get-vendor-spaces (vendor-id uint))
    (map-get? vendor-spaces { vendor-id: vendor-id })
)

(define-read-only (get-vendor-rating (vendor-id uint))
    (map-get? vendor-ratings { vendor-id: vendor-id })
)

(define-read-only (get-customer-review (reviewer principal) (vendor-id uint))
    (map-get? customer-reviews { reviewer: reviewer, vendor-id: vendor-id })
)

(define-read-only (is-market-active)
    (var-get market-active)
)

(define-read-only (get-space-rental-fee)
    (var-get space-rental-fee)
)

(define-read-only (get-contract-owner)
    CONTRACT_OWNER
)


;; title: farmers-market
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

