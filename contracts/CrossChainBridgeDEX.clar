;; CrossChain Bridge DEX Contract
;; A decentralized exchange enabling direct cross-chain trading without manual bridging
;; Supports multi-network token swaps with automated liquidity management

;; Define the native exchange token
(define-fungible-token bridge-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-invalid-chain (err u103))
(define-constant err-swap-failed (err u104))
(define-constant err-liquidity-insufficient (err u105))

;; Supported chain IDs
(define-constant STACKS_CHAIN u1)
(define-constant ETHEREUM_CHAIN u2)
(define-constant BITCOIN_CHAIN u3)

;; Exchange data variables
(define-data-var total-liquidity uint u0)
(define-data-var exchange-rate uint u1000000) ;; 1:1 default rate with 6 decimals
(define-data-var bridge-fee-rate uint u300) ;; 0.3% fee (300 basis points)

;; Cross-chain liquidity pools mapping: network-id -> token-amount
(define-map liquidity-pools uint uint)

;; User cross-chain balances: (user, network-id) -> balance
(define-map cross-chain-balances {user: principal, network-id: uint} uint)

;; Pending cross-chain transactions: tx-id -> transaction details
(define-map pending-swaps 
  uint 
  {
    sender: principal,
    source-chain: uint,
    target-chain: uint,
    amount: uint,
    fee: uint,
    timestamp: uint
  })

;; Transaction counter
(define-data-var next-tx-id uint u1)

;; Function 1: Cross-Chain Token Swap
;; Enables users to swap tokens across different blockchain networks
(define-public (cross-chain-swap 
  (amount uint) 
  (source-network uint) 
  (target-network uint) 
  (recipient principal))
  (let (
    (tx-id (var-get next-tx-id))
    (swap-fee (/ (* amount (var-get bridge-fee-rate)) u10000))
    (net-amount (- amount swap-fee))
    (source-liquidity (default-to u0 (map-get? liquidity-pools source-network)))
    (target-liquidity (default-to u0 (map-get? liquidity-pools target-network)))
  )
    ;; Validation checks
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (and (>= source-network u1) (<= source-network u3)) err-invalid-chain)
    (asserts! (and (>= target-network u1) (<= target-network u3)) err-invalid-chain)
    (asserts! (not (is-eq source-network target-network)) err-invalid-chain)
    (asserts! (>= target-liquidity net-amount) err-liquidity-insufficient)
    
    ;; Check user has sufficient balance on source chain
    (asserts! (>= (default-to u0 (map-get? cross-chain-balances 
                                  {user: tx-sender, network-id: source-network})) amount) 
              err-insufficient-balance)
    
    ;; Process the swap
    ;; Deduct from source chain
    (map-set cross-chain-balances 
             {user: tx-sender, network-id: source-network}
             (- (default-to u0 (map-get? cross-chain-balances 
                               {user: tx-sender, network-id: source-network})) amount))
    
    ;; Add liquidity to source pool
    (map-set liquidity-pools source-network (+ source-liquidity amount))
    
    ;; Deduct from target pool
    (map-set liquidity-pools target-network (- target-liquidity net-amount))
    
    ;; Credit recipient on target chain
    (map-set cross-chain-balances 
             {user: recipient, network-id: target-network}
             (+ (default-to u0 (map-get? cross-chain-balances 
                               {user: recipient, network-id: target-network})) net-amount))
    
    ;; Record pending transaction
    (map-set pending-swaps tx-id
             {
               sender: tx-sender,
               source-chain: source-network,
               target-chain: target-network,
               amount: amount,
               fee: swap-fee,
               timestamp: stacks-block-height
             })
    
    ;; Increment transaction counter
    (var-set next-tx-id (+ tx-id u1))
    
    ;; Emit event data
    (print {
      event: "cross-chain-swap",
      tx-id: tx-id,
      sender: tx-sender,
      recipient: recipient,
      source-chain: source-network,
      target-chain: target-network,
      amount: amount,
      fee: swap-fee,
      net-amount: net-amount
    })
    
    (ok tx-id)))

;; Function 2: Add Liquidity to Cross-Chain Pools
;; Allows liquidity providers to add funds to specific chain pools for earning fees
(define-public (add-liquidity (network-id uint) (amount uint))
  (let (
    (current-pool-balance (default-to u0 (map-get? liquidity-pools network-id)))
    (user-current-balance (default-to u0 (map-get? cross-chain-balances 
                                          {user: tx-sender, network-id: network-id})))
  )
    ;; Validation checks
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (and (>= network-id u1) (<= network-id u3)) err-invalid-chain)
    
    ;; Transfer tokens from user to contract (assuming they have STX or bridge tokens)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Mint equivalent bridge tokens to user on specified chain
    (try! (ft-mint? bridge-token amount tx-sender))
    
    ;; Update liquidity pool
    (map-set liquidity-pools network-id (+ current-pool-balance amount))
    
    ;; Update user balance on the chain
    (map-set cross-chain-balances 
             {user: tx-sender, network-id: network-id}
             (+ user-current-balance amount))
    
    ;; Update total liquidity
    (var-set total-liquidity (+ (var-get total-liquidity) amount))
    
    ;; Emit event
    (print {
      event: "liquidity-added",
      provider: tx-sender,
      network-id: network-id,
      amount: amount,
      new-pool-balance: (+ current-pool-balance amount)
    })
    
    (ok true)))

;; Read-only functions for querying contract state

;; Get user balance on specific chain
(define-read-only (get-cross-chain-balance (user principal) (network-id uint))
  (ok (default-to u0 (map-get? cross-chain-balances {user: user, network-id: network-id}))))

;; Get liquidity pool balance for specific chain
(define-read-only (get-pool-liquidity (network-id uint))
  (ok (default-to u0 (map-get? liquidity-pools network-id))))

;; Get pending swap details
(define-read-only (get-pending-swap (tx-id uint))
  (ok (map-get? pending-swaps tx-id)))

;; Get current exchange rate
(define-read-only (get-exchange-rate)
  (ok (var-get exchange-rate)))

;; Get bridge fee rate
(define-read-only (get-bridge-fee-rate)
  (ok (var-get bridge-fee-rate)))

;; Get total liquidity across all chains
(define-read-only (get-total-liquidity)
  (ok (var-get total-liquidity)))

;; Admin function to update exchange rates (owner only)
(define-public (update-exchange-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-rate u0) err-invalid-amount)
    (var-set exchange-rate new-rate)
    (ok true)))

;; Emergency function to pause/update fee rates (owner only)
(define-public (update-bridge-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount) ;; Max 10% fee
    (var-set bridge-fee-rate new-fee-rate)
    (ok true)))