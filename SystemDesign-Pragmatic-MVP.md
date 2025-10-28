# IntelLedger Pragmatic MVP - System Design

## 1. System Architecture

### 1.1 High-Level Architecture (Simplified)

```
┌─────────────────────────────────────────────────────────┐
│                    CLIENT LAYER                         │
│              Next.js 14 Web Application                 │
│         (Dashboard + Device Management + Charts)        │
└─────────────────────────────────────────────────────────┘
                         ↓ HTTPS
┌─────────────────────────────────────────────────────────┐
│                   API LAYER (tRPC)                      │
│              Node.js Monolith on Railway                │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐             │
│  │ Devices  │  │    ML    │  │Blockchain│             │
│  │  Router  │  │  Router  │  │  Router  │             │
│  └──────────┘  └──────────┘  └──────────┘             │
└─────────────────────────────────────────────────────────┘
         ↓              ↓              ↓
┌─────────────────────────────────────────────────────────┐
│                   DATA LAYER                            │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐             │
│  │PostgreSQL│  │  Redis   │  │   MQTT   │             │
│  │(Supabase)│  │ (Cache)  │  │ (HiveMQ) │             │
│  └──────────┘  └──────────┘  └──────────┘             │
└─────────────────────────────────────────────────────────┘
         ↓              ↓              ↓
┌─────────────────────────────────────────────────────────┐
│                INFRASTRUCTURE LAYER                     │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐             │
│  │  ESP32   │  │  Python  │  │ Polygon  │             │
│  │ Devices  │  │ ML API   │  │  Mumbai  │             │
│  │(MicroPy) │  │(Railway) │  │(Testnet) │             │
│  └──────────┘  └──────────┘  └──────────┘             │
└─────────────────────────────────────────────────────────┘
```

## 2. Component Design

### 2.1 IoT Device Layer (ESP32)

**Hardware Configuration:**
```
┌─────────────────────────────────────┐
│         ESP32-S3 Device             │
│  ┌───────────────────────────────┐ │
│  │  MicroPython Runtime          │ │
│  │  ┌─────────┐  ┌─────────┐    │ │
│  │  │ main.py │  │ config  │    │ │
│  │  └─────────┘  └─────────┘    │ │
│  │  ┌─────────┐  ┌─────────┐    │ │
│  │  │ sensors │  │  mqtt   │    │ │
│  │  └─────────┘  └─────────┘    │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │  Hardware                     │ │
│  │  - DHT22 (Temp/Humidity)      │ │
│  │  - MPU6050 (Accelerometer)    │ │
│  │  - WiFi Module                │ │
│  └───────────────────────────────┘ │
└─────────────────────────────────────┘
```

**State Machine:**
```
[BOOT] → [WIFI_CONNECT] → [MQTT_CONNECT] → [READY]
                ↓               ↓              ↓
           [RETRY_WIFI]    [RETRY_MQTT]   [READ_LOOP]
                                              ↓
                                         [PUBLISH]
                                              ↓
                                         [SLEEP(10s)]
```

**Data Structure:**
```python
{
  "device_id": "esp32-001",
  "timestamp": 1698765432,
  "temperature": 23.5,
  "humidity": 65.2,
  "accel_x": 0.12,
  "accel_y": -0.05,
  "accel_z": 9.81
}
```

### 2.2 Backend API (Node.js + tRPC)

**Project Structure:**
```
backend/
├── src/
│   ├── server.ts              # Main server
│   ├── routers/
│   │   ├── devices.ts         # Device CRUD
│   │   ├── ml.ts              # ML inference
│   │   ├── proofs.ts          # ZKP operations
│   │   └── blockchain.ts      # Smart contract calls
│   ├── services/
│   │   ├── mqtt.ts            # MQTT listener
│   │   ├── ml-client.ts       # HTTP client to ML service
│   │   └── blockchain.ts      # ethers.js wrapper
│   └── prisma/
│       └── schema.prisma      # Database schema
└── package.json
```

**API Design (tRPC):**
```typescript
// Type-safe API routes
AppRouter = {
  devices: {
    list: () => Device[]
    register: (input) => Device
    getData: (deviceId) => SensorReading[]
    getStatus: (deviceId) => DeviceStatus
  }
  
  ml: {
    predict: (input) => Prediction
    getModelInfo: () => ModelInfo
  }
  
  blockchain: {
    attestData: (input) => Transaction
    getAttestations: (deviceId) => Attestation[]
  }
  
  proofs: {
    generate: (input) => Proof
    verify: (proof) => boolean
  }
}
```

**Database Schema (Prisma):**
```prisma
model Device {
  id        String   @id @default(cuid())
  deviceId  String   @unique
  name      String
  status    String   @default("active")
  createdAt DateTime @default(now())
  readings  SensorReading[]
}

model SensorReading {
  id          String   @id @default(cuid())
  deviceId    String
  device      Device   @relation(fields: [deviceId], references: [deviceId])
  timestamp   DateTime
  temperature Float
  humidity    Float
  accelX      Float?
  accelY      Float?
  accelZ      Float?
  prediction  String?
  
  @@index([deviceId, timestamp])
}

model Attestation {
  id        String   @id @default(cuid())
  deviceId  String
  dataHash  String
  txHash    String
  timestamp DateTime @default(now())
  
  @@index([deviceId])
}
```

### 2.3 ML Service (Python FastAPI)

**Service Architecture:**
```
┌─────────────────────────────────────┐
│      ML Service (FastAPI)           │
│  ┌───────────────────────────────┐ │
│  │  Endpoints:                   │ │
│  │  POST /predict                │ │
│  │  POST /train                  │ │
│  │  GET  /health                 │ │
│  │  GET  /model-info             │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │  ONNX Runtime                 │ │
│  │  - Load model.onnx            │ │
│  │  - Run inference              │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │  Model Storage                │ │
│  │  - model.onnx (local)         │ │
│  └───────────────────────────────┘ │
└─────────────────────────────────────┘
```

**Request/Response Flow:**
```
Request:
{
  "temperature": 23.5,
  "humidity": 65.2
}

↓ Preprocessing
[23.5, 65.2] → normalize → [0.235, 0.652]

↓ Inference (ONNX)
model.run(input) → [0.8, 0.15, 0.05]

↓ Postprocessing
argmax → class 0 → "normal"

Response:
{
  "prediction": "normal",
  "confidence": 0.8
}
```

### 2.4 ZKP Service (Simplified)

**Circuit Design:**
```circom
// Simple sensor attestation
template SensorCheck() {
    signal input value;
    signal input threshold;
    signal output valid;
    
    // Constraint: value < threshold
    component lt = LessThan(32);
    lt.in[0] <== value;
    lt.in[1] <== threshold;
    
    valid <== lt.out;
}

component main = SensorCheck();
```

**Proof Generation Flow:**
```
Input → Validate → Generate Witness → 
Prove (SnarkJS) → Store Proof → Return Hash

Timing:
- Witness generation: 100ms
- Proof generation: 2-5s
- Verification: 10ms
```

### 2.5 Blockchain Layer

**Smart Contract Design:**
```solidity
contract DeviceRegistry {
    // State
    mapping(string => Device) public devices;
    mapping(bytes32 => Attestation) public attestations;
    
    // Events
    event DeviceRegistered(string deviceId, address owner);
    event DataAttested(string deviceId, bytes32 dataHash);
    
    // Functions
    function registerDevice(string memory deviceId) external {
        require(!devices[deviceId].exists, "Device exists");
        devices[deviceId] = Device({
            owner: msg.sender,
            isActive: true,
            registeredAt: block.timestamp,
            exists: true
        });
        emit DeviceRegistered(deviceId, msg.sender);
    }
    
    function attestData(
        string memory deviceId,
        bytes32 dataHash
    ) external {
        require(devices[deviceId].exists, "Device not found");
        require(devices[deviceId].owner == msg.sender, "Not owner");
        
        bytes32 attestationId = keccak256(
            abi.encodePacked(deviceId, dataHash, block.timestamp)
        );
        
        attestations[attestationId] = Attestation({
            deviceId: deviceId,
            dataHash: dataHash,
            timestamp: block.timestamp,
            exists: true
        });
        
        emit DataAttested(deviceId, dataHash);
    }
}
```

**Deployment Configuration:**
```javascript
// hardhat.config.js
module.exports = {
  networks: {
    mumbai: {
      url: "https://rpc-mumbai.maticvigil.com",
      accounts: [process.env.PRIVATE_KEY],
      chainId: 80001
    }
  },
  solidity: "0.8.20"
};
```

### 2.6 Frontend (Next.js)

**Page Structure:**
```
app/
├── page.tsx                    # Dashboard (home)
├── devices/
│   ├── page.tsx               # Device list
│   └── [id]/
│       └── page.tsx           # Device detail
├── analytics/
│   └── page.tsx               # Charts & analytics
└── layout.tsx                 # Root layout
```

**Component Architecture:**
```
┌─────────────────────────────────────┐
│         Dashboard Page              │
│  ┌───────────────────────────────┐ │
│  │  Stats Cards                  │ │
│  │  (Devices, Readings, Temp)    │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │  Sensor Chart                 │ │
│  │  (Recharts line chart)        │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │  Device List                  │ │
│  │  (Table with status)          │ │
│  └───────────────────────────────┘ │
└─────────────────────────────────────┘
```

**Data Fetching (tRPC):**
```typescript
// Client-side data fetching
const { data: devices } = trpc.devices.list.useQuery();

const { data: readings } = trpc.devices.getData.useQuery(
  { deviceId: 'esp32-001' },
  { 
    refetchInterval: 5000,  // Poll every 5 seconds
    staleTime: 3000 
  }
);

// Optimistic updates
const registerMutation = trpc.devices.register.useMutation({
  onSuccess: () => {
    queryClient.invalidateQueries(['devices']);
  }
});
```

## 3. Data Flow Design

### 3.1 End-to-End Data Flow

```
[ESP32] → [MQTT] → [Backend] → [Database]
  10s      200ms      100ms       50ms
  
  ↓
[ML Service] → [Prediction]
   500ms         
  
  ↓
[Frontend] ← [tRPC Query]
              100ms
```

**Detailed Flow:**
```
1. ESP32 reads sensors (100ms)
2. Publish to MQTT (200ms)
3. Backend receives message (10ms)
4. Save to PostgreSQL (50ms)
5. Call ML service (500ms)
6. Update prediction (50ms)
7. Frontend polls (5s interval)
8. Display update (50ms)

Total latency: ~1 second (device to display)
```

### 3.2 MQTT Message Flow

```
Topic Structure:
  intelledger/{deviceId}/data
  intelledger/{deviceId}/status
  intelledger/{deviceId}/command

Message Format (JSON):
{
  "device_id": "esp32-001",
  "timestamp": 1698765432,
  "data": {
    "temperature": 23.5,
    "humidity": 65.2
  }
}

QoS: 1 (at least once delivery)
Retain: false
```

### 3.3 Blockchain Attestation Flow

```
1. Backend receives sensor data
2. Compute data hash: keccak256(data)
3. Call smart contract: attestData(deviceId, hash)
4. Wait for transaction confirmation (~2-5s)
5. Store txHash in database
6. Emit event to frontend

Cost: ~$0.01 per attestation (Polygon Mumbai)
```

## 4. Deployment Architecture

### 4.1 Service Deployment

```
┌─────────────────────────────────────────────┐
│              Vercel (Frontend)              │
│  - Auto-deploy on git push                  │
│  - Edge network (CDN)                       │
│  - Serverless functions                     │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│            Railway (Backend API)            │
│  - Docker container                         │
│  - Auto-scaling                             │
│  - Environment variables                    │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│           Railway (ML Service)              │
│  - Python Docker container                  │
│  - 1GB RAM, 1 vCPU                          │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│           Supabase (Database)               │
│  - Managed PostgreSQL                       │
│  - Auto-backups                             │
│  - Connection pooling                       │
└─────────────────────────────────────────────┘
```

### 4.2 Environment Configuration

```bash
# Backend (.env)
DATABASE_URL="postgresql://user:pass@host:5432/db"
MQTT_BROKER="broker.hivemq.com"
MQTT_PORT=1883
ML_SERVICE_URL="https://ml-service.railway.app"
RPC_URL="https://rpc-mumbai.maticvigil.com"
PRIVATE_KEY="0x..."
CONTRACT_ADDRESS="0x..."

# Frontend (.env.local)
NEXT_PUBLIC_API_URL="https://api.intelledger.app"
NEXT_PUBLIC_CHAIN_ID=80001
```

## 5. Security Design

### 5.1 Authentication (Simplified)

```
User Authentication:
  - Email/password (Supabase Auth)
  - JWT tokens
  - Session management

Device Authentication:
  - Device ID + API key
  - Stored in device firmware
  - Validated on MQTT connect
```

### 5.2 Security Layers

```
Layer 1: Network
  - HTTPS only (TLS 1.3)
  - MQTT over TLS
  - CORS configuration

Layer 2: Application
  - Input validation (Zod)
  - Rate limiting (basic)
  - SQL injection prevention (Prisma)

Layer 3: Data
  - Database encryption at rest
  - Environment variables for secrets
  - No sensitive data in logs
```

## 6. Monitoring & Observability

### 6.1 Logging

```
Backend Logs:
  - Console.log (Railway dashboard)
  - Error tracking (optional: Sentry)
  
Frontend Logs:
  - Vercel Analytics
  - Browser console (dev mode)
  
Device Logs:
  - Serial output (development)
  - MQTT status messages
```

### 6.2 Metrics (Basic)

```
Track:
  - API response times
  - Database query times
  - MQTT message count
  - ML inference latency
  - Blockchain transaction status

Tools:
  - Railway metrics (built-in)
  - Vercel Analytics
  - Custom logging
```

## 7. Scalability Design

### 7.1 Scaling Strategy

```
Current Capacity:
  - 10-50 devices
  - 100-500 requests/min
  - 1-5 concurrent users

Scaling Path:
  Phase 1 (MVP): Single instance
  Phase 2 (Beta): Add Redis caching
  Phase 3 (Production): Multiple instances + load balancer
```

### 7.2 Database Optimization

```
Indexes:
  - deviceId (for fast lookups)
  - timestamp (for time-series queries)
  - composite (deviceId, timestamp)

Query Optimization:
  - Limit results (LIMIT 100)
  - Pagination (offset/cursor)
  - Aggregate queries (GROUP BY)
```

## 8. Development Workflow

### 8.1 Local Development

```bash
# Terminal 1: Backend
cd backend
npm install
npx prisma migrate dev
npm run dev  # Port 3001

# Terminal 2: Frontend
cd frontend
npm install
npm run dev  # Port 3000

# Terminal 3: ML Service
cd ml-service
pip install -r requirements.txt
uvicorn main:app --reload  # Port 8000

# Terminal 4: Device (optional)
# Flash ESP32 with Thonny IDE
```

### 8.2 Testing Strategy

```
Unit Tests:
  - Backend: Vitest
  - Frontend: React Testing Library
  - ML: pytest

Integration Tests:
  - API endpoints (Supertest)
  - Database operations
  - Smart contract (Hardhat)

Manual Testing:
  - Device connectivity
  - Dashboard functionality
  - End-to-end flow
```

## 9. Performance Targets

```
| Metric                  | Target     |
| ----------------------- | ---------- |
| API Response Time       | < 500ms    |
| ML Inference Time       | < 1s       |
| Device Data Latency     | < 2s       |
| Dashboard Load Time     | < 3s       |
| MQTT Message Delivery   | < 500ms    |
| Blockchain Confirmation | < 10s      |
| System Uptime           | 95%+ (MVP) |
```

## 10. Cost Breakdown

### 10.1 Monthly Costs

```
| Service            | Cost         |
| ------------------ | ------------ |
| Vercel (Hobby)     | $0           |
| Railway (Starter)  | $5           |
| Supabase (Free)    | $0           |
| MQTT (HiveMQ Free) | $0           |
| Polygon Mumbai     | $0 (testnet) |
| Domain (optional)  | $12/year     |

Total: $5/month
```

### 10.2 One-Time Costs

```
| Item               | Cost |
| ------------------ | ---- |
| ESP32-S3 (×5)      | $50  |
| DHT22 sensors (×5) | $25  |
| MPU6050 (×5)       | $15  |
| Cables/breadboard  | $10  |

Total: $100
```

## 11. Migration Path

### 11.1 From MVP to Production

```
Phase 1: MVP (Current)
  - 5-10 devices
  - Free/cheap hosting
  - Testnet blockchain
  - Basic features

Phase 2: Beta (3-6 months)
  - 50-100 devices
  - Paid hosting ($125/month)
  - Mainnet blockchain
  - Authentication
  - Custom MQTT broker

Phase 3: Production (6-12 months)
  - 1000+ devices
  - Cloud infrastructure
  - Microservices
  - Advanced features
  - Choose: Industry Standard or ProofPerl
```

## 12. Quick Start Guide

### 12.1 Setup Steps

```bash
# 1. Clone repository
git clone https://github.com/yourusername/intelledger-mvp
cd intelledger-mvp

# 2. Install dependencies
npm install

# 3. Setup database
cd backend
npx prisma migrate dev
cd ..

# 4. Configure environment
cp .env.example .env
# Edit .env with your values

# 5. Deploy smart contracts
cd contracts
npx hardhat run scripts/deploy.ts --network mumbai
cd ..

# 6. Start services
npm run dev:all

# 7. Flash ESP32
# Use Thonny IDE to upload firmware/main.py

# 8. Open dashboard
# http://localhost:3000
```

### 12.2 First Device Registration

```bash
# 1. Power on ESP32
# 2. Check serial output for connection
# 3. Open dashboard
# 4. Click "Register Device"
# 5. Enter device ID: esp32-001
# 6. Save
# 7. Watch real-time data appear
```

This pragmatic MVP design gets you from zero to working system in 3-4 months with minimal complexity and cost.
