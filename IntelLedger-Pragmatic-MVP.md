# IntelLedger - Pragmatic MVP Architecture

## Executive Summary

This document outlines a **fast-to-build, functional MVP** of IntelLedger that balances modern best practices with rapid prototyping. The goal: working system in 3-4 months with a small team, covering 80% of functionality with 20% of the complexity.

---

## Design Philosophy

**Core Principles:**
1. **Use managed services** where possible (less ops overhead)
2. **Monorepo** for faster iteration
3. **TypeScript everywhere** (one language, easier hiring)
4. **Proven libraries** over custom solutions
5. **Deploy early, iterate fast**
6. **Good enough security** (not perfect, but solid)

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│              Frontend (Next.js + shadcn/ui)                  │
│         Real-time dashboard + Web3 wallet integration        │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│           Backend API (Node.js + tRPC + Prisma)              │
│         Single service, modular structure                    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌──────────────┬──────────────┬──────────────┬───────────────┐
│  IoT Layer   │   ML Layer   │  ZKP Layer   │ Blockchain    │
│              │              │              │ Layer         │
│ ESP32 +      │ Python       │ SnarkJS      │ Hardhat +     │
│ MicroPython  │ FastAPI      │ (simplified) │ Polygon       │
│ MQTT         │ ONNX Runtime │ Basic proofs │ Mumbai        │
└──────────────┴──────────────┴──────────────┴───────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│              Infrastructure (Vercel + Railway)               │
│         Managed hosting, zero DevOps                        │
└─────────────────────────────────────────────────────────────┘
```

---

## Tech Stack Summary

```yaml
Frontend: Next.js 14 + TypeScript + shadcn/ui
Backend: Node.js 20 + tRPC + Prisma
Database: PostgreSQL (Supabase)
IoT: ESP32 + MicroPython + MQTT
ML: Python + FastAPI + ONNX Runtime
ZKP: SnarkJS (basic circuits)
Blockchain: Hardhat + ethers.js + Polygon Mumbai
Hosting: Vercel (frontend) + Railway (backend)
```

---

## Layer 1: IoT/Edge Layer (Simplified)

### Hardware

**Single Device Type:**
- **ESP32-S3-DevKitC-1** ($10/unit)
- **DHT22** temperature/humidity sensor ($5)
- **MPU6050** accelerometer ($3)
- Total: ~$18 per device

### Firmware (MicroPython)

**Simple, readable code:**

```python
# main.py - ESP32 firmware
import machine
import network
import time
import ujson
from umqtt.simple import MQTTClient
import dht
import mpu6050

# Configuration
WIFI_SSID = "your-wifi"
WIFI_PASSWORD = "your-password"
MQTT_BROKER = "broker.hivemq.com"  # Free public broker for MVP
DEVICE_ID = "esp32-001"

# Initialize sensors
dht_sensor = dht.DHT22(machine.Pin(4))
i2c = machine.I2C(0, scl=machine.Pin(22), sda=machine.Pin(21))
mpu = mpu6050.MPU6050(i2c)

# Connect to WiFi
def connect_wifi():
    wlan = network.WLAN(network.STA_IF)
    wlan.active(True)
    wlan.connect(WIFI_SSID, WIFI_PASSWORD)
    while not wlan.isconnected():
        time.sleep(1)
    print("Connected:", wlan.ifconfig())

# Read sensors
def read_sensors():
    dht_sensor.measure()
    temp = dht_sensor.temperature()
    humidity = dht_sensor.humidity()
    accel = mpu.get_accel_data()
    
    return {
        "device_id": DEVICE_ID,
        "timestamp": time.time(),
        "temperature": temp,
        "humidity": humidity,
        "accel_x": accel['x'],
        "accel_y": accel['y'],
        "accel_z": accel['z']
    }

# Main loop
def main():
    connect_wifi()
    client = MQTTClient(DEVICE_ID, MQTT_BROKER)
    client.connect()
    
    while True:
        try:
            data = read_sensors()
            payload = ujson.dumps(data)
            client.publish(f"intelledger/{DEVICE_ID}/data", payload)
            print("Published:", payload)
            time.sleep(10)  # Send every 10 seconds
        except Exception as e:
            print("Error:", e)
            time.sleep(5)

if __name__ == "__main__":
    main()
```

**Deployment:**
- Flash with Thonny IDE (beginner-friendly)
- OTA updates via WebREPL (built-in)
- No custom build system needed

---

## Layer 2: Backend API (Node.js Monolith)

### Project Structure

```
backend/
├── src/
│   ├── server.ts           # Express + tRPC server
│   ├── routers/
│   │   ├── devices.ts      # Device management
│   │   ├── ml.ts           # ML inference
│   │   ├── proofs.ts       # ZKP generation
│   │   └── blockchain.ts   # Smart contract calls
│   ├── services/
│   │   ├── mqtt.ts         # MQTT client
│   │   ├── ml-client.ts    # Call Python ML service
│   │   └── blockchain.ts   # ethers.js wrapper
│   └── prisma/
│       └── schema.prisma   # Database schema
├── package.json
└── tsconfig.json
```

### Core Implementation

**tRPC API (Type-safe):**
```typescript
// src/server.ts
import { initTRPC } from '@trpc/server';
import { createHTTPServer } from '@trpc/server/adapters/standalone';
import { z } from 'zod';
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();
const t = initTRPC.create();

// Device router
const deviceRouter = t.router({
  list: t.procedure.query(async () => {
    return prisma.device.findMany();
  }),
  
  register: t.procedure
    .input(z.object({
      deviceId: z.string(),
      name: z.string(),
    }))
    .mutation(async ({ input }) => {
      return prisma.device.create({
        data: {
          deviceId: input.deviceId,
          name: input.name,
          status: 'active',
        },
      });
    }),
  
  getData: t.procedure
    .input(z.object({ deviceId: z.string() }))
    .query(async ({ input }) => {
      return prisma.sensorReading.findMany({
        where: { deviceId: input.deviceId },
        orderBy: { timestamp: 'desc' },
        take: 100,
      });
    }),
});

// ML router
const mlRouter = t.router({
  predict: t.procedure
    .input(z.object({
      temperature: z.number(),
      humidity: z.number(),
    }))
    .mutation(async ({ input }) => {
      // Call Python ML service
      const response = await fetch('http://ml-service:8000/predict', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(input),
      });
      return response.json();
    }),
});

// Blockchain router
const blockchainRouter = t.router({
  attestData: t.procedure
    .input(z.object({
      dataHash: z.string(),
      deviceId: z.string(),
    }))
    .mutation(async ({ input }) => {
      const { ethers } = await import('ethers');
      const provider = new ethers.JsonRpcProvider(process.env.RPC_URL);
      const wallet = new ethers.Wallet(process.env.PRIVATE_KEY!, provider);
      const contract = new ethers.Contract(
        process.env.CONTRACT_ADDRESS!,
        contractABI,
        wallet
      );
      
      const tx = await contract.attestData(input.dataHash, input.deviceId);
      await tx.wait();
      
      return { txHash: tx.hash };
    }),
});

// Main router
const appRouter = t.router({
  devices: deviceRouter,
  ml: mlRouter,
  blockchain: blockchainRouter,
});

export type AppRouter = typeof appRouter;

// Start server
createHTTPServer({
  router: appRouter,
}).listen(3001);
```

**Database Schema (Prisma):**
```prisma
// prisma/schema.prisma
datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

generator client {
  provider = "prisma-client-js"
}

model Device {
  id        String   @id @default(cuid())
  deviceId  String   @unique
  name      String
  status    String
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
  dataHash  String
  deviceId  String
  txHash    String
  timestamp DateTime @default(now())
  
  @@index([deviceId])
}
```

**MQTT Listener:**
```typescript
// src/services/mqtt.ts
import mqtt from 'mqtt';
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();
const client = mqtt.connect('mqtt://broker.hivemq.com');

client.on('connect', () => {
  console.log('Connected to MQTT broker');
  client.subscribe('intelledger/+/data');
});

client.on('message', async (topic, message) => {
  try {
    const data = JSON.parse(message.toString());
    
    // Save to database
    await prisma.sensorReading.create({
      data: {
        deviceId: data.device_id,
        timestamp: new Date(data.timestamp * 1000),
        temperature: data.temperature,
        humidity: data.humidity,
        accelX: data.accel_x,
        accelY: data.accel_y,
        accelZ: data.accel_z,
      },
    });
    
    console.log('Saved reading:', data.device_id);
  } catch (error) {
    console.error('Error processing message:', error);
  }
});
```

---

## Layer 3: ML Service (Python FastAPI)

### Simple ML Service

```python
# ml-service/main.py
from fastapi import FastAPI
from pydantic import BaseModel
import onnxruntime as ort
import numpy as np

app = FastAPI()

# Load pre-trained ONNX model
session = ort.InferenceSession("model.onnx")

class PredictionInput(BaseModel):
    temperature: float
    humidity: float

class PredictionOutput(BaseModel):
    prediction: str
    confidence: float

@app.post("/predict", response_model=PredictionOutput)
async def predict(input: PredictionInput):
    # Prepare input
    features = np.array([[input.temperature, input.humidity]], dtype=np.float32)
    
    # Run inference
    outputs = session.run(None, {"input": features})
    prediction = outputs[0][0]
    confidence = float(outputs[1][0].max())
    
    # Map to label
    labels = ["normal", "warning", "critical"]
    result = labels[int(prediction)]
    
    return PredictionOutput(
        prediction=result,
        confidence=confidence
    )

@app.get("/health")
async def health():
    return {"status": "ok"}

# Train simple model (for demo)
@app.post("/train")
async def train():
    from sklearn.ensemble import RandomForestClassifier
    from sklearn.datasets import make_classification
    from skl2onnx import convert_sklearn
    from skl2onnx.common.data_types import FloatTensorType
    
    # Generate synthetic data
    X, y = make_classification(n_samples=1000, n_features=2, n_classes=3)
    
    # Train model
    model = RandomForestClassifier(n_estimators=10)
    model.fit(X, y)
    
    # Convert to ONNX
    initial_type = [('input', FloatTensorType([None, 2]))]
    onnx_model = convert_sklearn(model, initial_types=initial_type)
    
    # Save
    with open("model.onnx", "wb") as f:
        f.write(onnx_model.SerializeToString())
    
    return {"status": "trained"}
```

**Dockerfile:**
```dockerfile
FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
```

---

## Layer 4: ZKP Layer (Simplified)

### Basic SnarkJS Circuit

**Simple sensor attestation:**
```circom
// circuits/sensor.circom
pragma circom 2.0.0;

template SensorCheck() {
    signal input value;
    signal input threshold;
    signal output valid;
    
    // Check if value < threshold
    signal diff;
    diff <== threshold - value;
    
    // Output 1 if valid, 0 otherwise
    valid <== diff * diff;  // Simplified for MVP
}

component main = SensorCheck();
```

**Generate proof (Node.js):**
```typescript
// src/services/zkp.ts
import { groth16 } from 'snarkjs';
import fs from 'fs';

export async function generateProof(value: number, threshold: number) {
  const input = {
    value: value,
    threshold: threshold,
  };
  
  // Generate proof
  const { proof, publicSignals } = await groth16.fullProve(
    input,
    'circuits/sensor.wasm',
    'circuits/sensor_final.zkey'
  );
  
  return { proof, publicSignals };
}

export async function verifyProof(proof: any, publicSignals: any) {
  const vKey = JSON.parse(fs.readFileSync('circuits/verification_key.json', 'utf-8'));
  const verified = await groth16.verify(vKey, publicSignals, proof);
  return verified;
}
```

**Setup (one-time):**
```bash
# Compile circuit
circom sensor.circom --r1cs --wasm --sym

# Generate proving key (use existing ceremony for MVP)
snarkjs groth16 setup sensor.r1cs pot12_final.ptau sensor_0000.zkey

# Export verification key
snarkjs zkey export verificationkey sensor_0000.zkey verification_key.json

# Generate Solidity verifier
snarkjs zkey export solidityverifier sensor_0000.zkey verifier.sol
```

---

## Layer 5: Blockchain Layer (Hardhat)

### Smart Contracts

**Device Registry:**
```solidity
// contracts/DeviceRegistry.sol
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract DeviceRegistry {
    struct Device {
        address owner;
        string deviceId;
        bool isActive;
        uint256 registeredAt;
    }
    
    mapping(string => Device) public devices;
    
    event DeviceRegistered(string deviceId, address owner);
    event DataAttested(string deviceId, bytes32 dataHash, uint256 timestamp);
    
    function registerDevice(string memory deviceId) external {
        require(bytes(devices[deviceId].deviceId).length == 0, "Device exists");
        
        devices[deviceId] = Device({
            owner: msg.sender,
            deviceId: deviceId,
            isActive: true,
            registeredAt: block.timestamp
        });
        
        emit DeviceRegistered(deviceId, msg.sender);
    }
    
    function attestData(string memory deviceId, bytes32 dataHash) external {
        require(devices[deviceId].isActive, "Device not active");
        require(devices[deviceId].owner == msg.sender, "Not owner");
        
        emit DataAttested(deviceId, dataHash, block.timestamp);
    }
    
    function isDeviceActive(string memory deviceId) external view returns (bool) {
        return devices[deviceId].isActive;
    }
}
```

**Hardhat Config:**
```typescript
// hardhat.config.ts
import { HardhatUserConfig } from "hardhat/config";
import "@nomicfoundation/hardhat-toolbox";

const config: HardhatUserConfig = {
  solidity: "0.8.20",
  networks: {
    mumbai: {
      url: process.env.MUMBAI_RPC_URL || "",
      accounts: process.env.PRIVATE_KEY ? [process.env.PRIVATE_KEY] : [],
    },
  },
  etherscan: {
    apiKey: process.env.POLYGONSCAN_API_KEY,
  },
};

export default config;
```

**Deployment Script:**
```typescript
// scripts/deploy.ts
import { ethers } from "hardhat";

async function main() {
  const DeviceRegistry = await ethers.getContractFactory("DeviceRegistry");
  const registry = await DeviceRegistry.deploy();
  await registry.waitForDeployment();
  
  console.log("DeviceRegistry deployed to:", await registry.getAddress());
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
```

---

## Layer 6: Frontend (Next.js)

### Project Structure

```
frontend/
├── app/
│   ├── page.tsx              # Dashboard
│   ├── devices/
│   │   └── page.tsx          # Device list
│   ├── analytics/
│   │   └── page.tsx          # Charts
│   └── layout.tsx
├── components/
│   ├── device-card.tsx
│   ├── sensor-chart.tsx
│   └── wallet-connect.tsx
├── lib/
│   ├── trpc.ts               # tRPC client
│   └── wagmi.ts              # Web3 config
└── package.json
```

### Key Components

**Dashboard:**
```typescript
// app/page.tsx
'use client';

import { trpc } from '@/lib/trpc';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { SensorChart } from '@/components/sensor-chart';

export default function Dashboard() {
  const { data: devices } = trpc.devices.list.useQuery();
  const { data: readings } = trpc.devices.getData.useQuery(
    { deviceId: 'esp32-001' },
    { refetchInterval: 5000 } // Poll every 5 seconds
  );
  
  return (
    <div className="p-8">
      <h1 className="text-3xl font-bold mb-8">IntelLedger Dashboard</h1>
      
      <div className="grid grid-cols-3 gap-4 mb-8">
        <Card>
          <CardHeader>
            <CardTitle>Active Devices</CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-4xl font-bold">{devices?.length || 0}</p>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader>
            <CardTitle>Total Readings</CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-4xl font-bold">{readings?.length || 0}</p>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader>
            <CardTitle>Latest Temp</CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-4xl font-bold">
              {readings?.[0]?.temperature.toFixed(1)}°C
            </p>
          </CardContent>
        </Card>
      </div>
      
      <Card>
        <CardHeader>
          <CardTitle>Temperature Over Time</CardTitle>
        </CardHeader>
        <CardContent>
          <SensorChart data={readings || []} />
        </CardContent>
      </Card>
    </div>
  );
}
```

**Sensor Chart:**
```typescript
// components/sensor-chart.tsx
'use client';

import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';

export function SensorChart({ data }: { data: any[] }) {
  const chartData = data.map(reading => ({
    time: new Date(reading.timestamp).toLocaleTimeString(),
    temperature: reading.temperature,
    humidity: reading.humidity,
  }));
  
  return (
    <ResponsiveContainer width="100%" height={300}>
      <LineChart data={chartData}>
        <CartesianGrid strokeDasharray="3 3" />
        <XAxis dataKey="time" />
        <YAxis />
        <Tooltip />
        <Line type="monotone" dataKey="temperature" stroke="#8884d8" />
        <Line type="monotone" dataKey="humidity" stroke="#82ca9d" />
      </LineChart>
    </ResponsiveContainer>
  );
}
```

**Web3 Integration:**
```typescript
// components/wallet-connect.tsx
'use client';

import { useAccount, useConnect, useDisconnect } from 'wagmi';
import { Button } from '@/components/ui/button';

export function WalletConnect() {
  const { address, isConnected } = useAccount();
  const { connect, connectors } = useConnect();
  const { disconnect } = useDisconnect();
  
  if (isConnected) {
    return (
      <div className="flex items-center gap-4">
        <span className="text-sm">{address?.slice(0, 6)}...{address?.slice(-4)}</span>
        <Button onClick={() => disconnect()}>Disconnect</Button>
      </div>
    );
  }
  
  return (
    <Button onClick={() => connect({ connector: connectors[0] })}>
      Connect Wallet
    </Button>
  );
}
```

---

## Infrastructure & Deployment

### Managed Services (Zero DevOps)

**Frontend (Vercel):**
```bash
# Deploy with one command
vercel deploy --prod

# Auto-deploys on git push to main
```

**Backend (Railway):**
```yaml
# railway.json
{
  "build": {
    "builder": "NIXPACKS"
  },
  "deploy": {
    "startCommand": "npm start",
    "restartPolicyType": "ON_FAILURE"
  }
}
```

**Database (Supabase):**
- Free tier: 500MB database
- Auto-backups
- Built-in auth (optional)
- Real-time subscriptions

**ML Service (Railway):**
```dockerfile
# Dockerfile
FROM python:3.11-slim
WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY . .
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
```

### Environment Variables

```bash
# .env
DATABASE_URL="postgresql://..."
MQTT_BROKER="broker.hivemq.com"
RPC_URL="https://rpc-mumbai.maticvigil.com"
PRIVATE_KEY="0x..."
CONTRACT_ADDRESS="0x..."
ML_SERVICE_URL="https://ml-service.railway.app"
```

---

## Development Workflow

### Local Development

```bash
# 1. Clone repo
git clone https://github.com/yourusername/intelledger-mvp
cd intelledger-mvp

# 2. Install dependencies
npm install

# 3. Setup database
npx prisma migrate dev

# 4. Start services
npm run dev:backend  # Port 3001
npm run dev:frontend # Port 3000
npm run dev:ml       # Port 8000

# 5. Flash ESP32
# Use Thonny IDE to upload main.py
```

### Testing

```typescript
// Simple integration test
import { test, expect } from 'vitest';
import { appRouter } from './server';

test('register device', async () => {
  const caller = appRouter.createCaller({});
  
  const device = await caller.devices.register({
    deviceId: 'test-001',
    name: 'Test Device',
  });
  
  expect(device.deviceId).toBe('test-001');
});
```

---

## Cost Estimation

### Monthly Costs (MVP)

```yaml
Hosting:
  - Vercel (Hobby): $0
  - Railway (Starter): $5
  - Supabase (Free): $0
  Total: $5/month

Blockchain:
  - Polygon Mumbai (testnet): $0
  - RPC (Alchemy free tier): $0
  Total: $0/month

IoT:
  - MQTT (HiveMQ free): $0
  - Devices (one-time): $20 × 5 = $100
  Total: $100 one-time

Grand Total: $5/month + $100 setup
```

### Production Upgrade Path

```yaml
When scaling to production:
  - Vercel Pro: $20/month
  - Railway Pro: $20/month
  - Supabase Pro: $25/month
  - Polygon Mainnet gas: ~$50/month
  - Custom MQTT broker: $10/month
  Total: ~$125/month
```

---

## Team & Timeline

### Minimal Team

```yaml
Team (2-3 people):
  - 1 × Full-stack Developer (TypeScript)
  - 1 × IoT/Embedded Developer
  - 0.5 × Designer (part-time)

Timeline: 3-4 months to working MVP

Breakdown:
  - Month 1: IoT + Backend API
  - Month 2: ML + Frontend
  - Month 3: Blockchain + ZKP
  - Month 4: Polish + Testing
```

---

## What's Included vs. Excluded

### ✅ Included (MVP)

- Device registration & management
- Real-time sensor data collection
- Basic ML inference (anomaly detection)
- Simple ZKP (sensor attestation)
- Blockchain data attestation
- Web dashboard with charts
- Wallet integration

### ❌ Excluded (Future)

- Formal verification
- Hardware attestation
- Complex ZK circuits
- Recursive proofs
- Custom blockchain
- Advanced ML (federated learning)
- Mobile app
- Multi-tenancy
- Advanced analytics

---

## Pros & Cons

### Advantages ✅

- **Fast:** 3-4 months to working system
- **Cheap:** $5/month + $100 setup
- **Simple:** One language (TypeScript) for most code
- **Proven:** All libraries battle-tested
- **Scalable:** Easy upgrade path to production
- **Learnable:** Junior devs can contribute
- **Deployable:** Push to deploy (Vercel/Railway)

### Disadvantages ❌

- **Less secure:** No formal verification
- **Vendor lock-in:** Depends on managed services
- **Limited scale:** Not for millions of devices
- **Basic ZKP:** Simple circuits only
- **No hardware trust:** Software-only security
- **Testnet only:** Not production blockchain

---

## Migration Path

### From MVP to Production

**Phase 1: MVP (3-4 months)**
- Current architecture
- 5-10 devices
- Testnet only
- Free/cheap hosting

**Phase 2: Beta (6-9 months)**
- Add authentication
- Custom MQTT broker
- Mainnet deployment
- 50-100 devices
- Paid hosting ($125/month)

**Phase 3: Production (12+ months)**
- Microservices architecture
- Kubernetes deployment
- Advanced ZKP circuits
- 1000+ devices
- Full monitoring
- Choose: Industry Standard or ProofPerl path

---

## Conclusion

This pragmatic MVP gives you a **working IntelLedger system in 3-4 months** with a tiny team and minimal budget. It covers the core functionality while keeping complexity low.

**Perfect for:**
- Proof of concept
- Investor demos
- Early customer validation
- Learning the domain
- Testing market fit

**Not for:**
- Production critical infrastructure
- High-security requirements
- Massive scale (1M+ devices)
- Regulatory compliance

Once validated, you can migrate to either the Industry Standard (fast scale) or ProofPerl (maximum security) architecture.

---

## Quick Start

```bash
# 1. Clone starter template
git clone https://github.com/yourusername/intelledger-mvp-starter
cd intelledger-mvp-starter

# 2. Install dependencies
npm install

# 3. Setup environment
cp .env.example .env
# Edit .env with your values

# 4. Initialize database
npx prisma migrate dev

# 5. Deploy contracts
cd contracts
npx hardhat run scripts/deploy.ts --network mumbai

# 6. Start development
npm run dev

# 7. Flash ESP32
# Open Thonny, connect ESP32, upload firmware/main.py

# 8. Open dashboard
# http://localhost:3000
```

**You're live in under 30 minutes!** 🚀
