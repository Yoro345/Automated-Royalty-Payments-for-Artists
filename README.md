# 🎵 Automated Royalty Payments for Artists

> Transparent, automated music royalty distribution powered by Stacks blockchain smart contracts

## 🚀 Overview

This smart contract solves the problem of opaque music royalty systems by automatically splitting streaming revenue among artists using blockchain technology. Musicians can register, create revenue streams, and receive their fair share of earnings without intermediaries.

## ✨ Features

- 🎤 **Artist Registration**: Musicians can register with custom royalty percentages
- 💰 **Revenue Streams**: Create and manage multiple income sources
- 🔄 **Automatic Distribution**: Smart contracts handle royalty splits automatically
- 📊 **Transparent Tracking**: All payments and distributions are recorded on-chain
- 🛡️ **Secure**: Owner controls and emergency functions for safety
- 💸 **Low Fees**: Configurable contract fees (default 2.5%)

## 🛠️ Installation

1. Clone the repository:
```bash
git clone https://github.com/your-username/Automated-Royalty-Payments-for-Artists.git
cd Automated-Royalty-Payments-for-Artists
```

2. Install dependencies:
```bash
npm install
```

3. Start Clarinet console:
```bash
clarinet console
```

## 📖 Usage

### 🎨 Register as an Artist

```clarity
(contract-call? .automated-royalty-artists register-artist "Artist Name" u2500)
```
- `name`: Artist name (max 50 characters)
- `royalty-percentage`: Percentage in basis points (2500 = 25%)

### 🎶 Create a Revenue Stream

```clarity
(contract-call? .automated-royalty-artists create-revenue-stream "Song Title - Streaming Revenue")
```

### 👥 Add Artists to Revenue Stream

```clarity
(contract-call? .automated-royalty-artists add-artist-to-stream u1 u1 u5000)
```
- `stream-id`: ID of the revenue stream
- `artist-id`: ID of the artist
- `percentage`: Artist's share in basis points (5000 = 50%)

### 💵 Add Revenue to Stream

```clarity
(contract-call? .automated-royalty-artists add-revenue u1)
```
Transfers your STX balance to the contract for distribution.

### 💰 Distribute Royalties

```clarity
(contract-call? .automated-royalty-artists distribute-royalties u1 u1)
```
Automatically calculates and sends payment to the artist.

## 🔍 Read-Only Functions

### Get Artist Information
```clarity
(contract-call? .automated-royalty-artists get-artist u1)
(contract-call? .automated-royalty-artists get-artist-by-principal 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE)
```

### Get Revenue Stream Details
```clarity
(contract-call? .automated-royalty-artists get-revenue-stream u1)
```

### Calculate Payment
```clarity
(contract-call? .automated-royalty-artists calculate-artist-payment u1 u1 u1000000)
```

### Check Contract Balance
```clarity
(contract-call? .automated-royalty-artists get-contract-balance)
```

## 📊 Data Structures

### Artist
```clarity
{
  owner: principal,
  name: string-ascii,
  royalty-percentage: uint,
  total-earned: uint,
  is-active: bool,
  created-at: uint
}
```

### Revenue Stream
```clarity
{
  name: string-ascii,
  total-revenue: uint,
  distributed: uint,
  created-by: principal,
  created-at: uint,
  is-active: bool
}
```

### Payment History
```clarity
{
  artist-id: uint,
  stream-id: uint,
  amount: uint,
  timestamp: uint,
  tx-sender: principal
}
```

## 🔐 Admin Functions

### Set Contract Fee (Owner Only)
```clarity
(contract-call? .automated-royalty-artists set-contract-fee u100)
```
Sets fee in basis points (100 = 1%).

### Emergency Withdraw (Owner Only)
```clarity
(contract-call? .automated-royalty-artists emergency-withdraw)
```

## ⚠️ Error Codes

- `u100`: Owner only operation
- `u101`: Not found
- `u102`: Already exists
- `u103`: Invalid percentage
- `u104`: Insufficient funds
- `u105`: Unauthorized
- `u106`: Invalid amount

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details.

## 🙋 Support

For questions or support, please open an issue on GitHub.

---

*Built with ❤️ for the music community*
