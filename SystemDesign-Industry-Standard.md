# IntelLedger Industry Standard - System Design

## 1. System Architecture

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         CLIENT TIER                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │   Web App    │  │  Mobile App  │  │   Admin      │          │
│  │  (Next.js)   │  │  (React N.)  │  │  Dashboard   │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
└─────────────────────────────────────────────────────────────────┘
                              ↓ HTTPS/WSS
┌─────────────────────────────────────────────────────────────────┐
│                      API GATEWAY TIER                            │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Kong Gateway (Rate Limit, Auth, CORS, Logging)         │  │
│  │  + Auth0 (OAuth 2.0 / OpenID Connect)                   │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                    APPLICATION TIER (K8s)                        │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │ Device   │  │   ML     │  │   ZKP    │  │Blockchain│       │
│  │ Service  │  │ Service  │  │ Service  │  │ Service  │       │
│  │(NestJS)  │  │(FastAPI) │  │(Node.js) │  │(Node.js) │       │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                      DATA TIER                                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │PostgreSQL│  │TimescaleDB│ │  Redis   │  │  Kafka   │       │
│  │  (RDS)   │  │(Time-ser.)│ │ (Cache)  │  │(Streaming)│      │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                    INFRASTRUCTURE TIER                           │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│  │ IoT Edge │  │ ML Infra │  │ZKP Compute│ │Blockchain│       │
│  │AWS IoT   │  │SageMaker │  │GPU Cluster│ │Polygon   │       │
│  │Greengrass│  │TorchServe│  │SnarkJS   │  │The Graph │       │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
└─────────────────────────────────────────────────────────────────┘
```

## 2. Component Design

### 2.1 IoT Edge Layer

**Component Diagram:**
```
┌─────────────────────────────────────────┐
│         ESP32-S3 Device                 │
│  ┌───────────────────────────────────┐ │
│  │  Application Layer (FreeRTOS)     │ │
│  │  ┌─────────┐  ┌─────────────┐    │ │
│  │  │ Sensor  │  │   MQTT      │    │ │
│  │  │ Manager │  │   Client    │    │ │
│  │  └─────────┘  └─────────────┘    │ │
│  │  ┌─────────┐  ┌─────────────┐    │ │
│  │  │  Edge   │  │   TLS       │    │ │
│  │  │   ML    │  │   Stack     │    │ │
│  │  └─────────┘  └─────────────┘    │ │
│  └───────────────────────────────────┘ │
│  ┌───────────────────────────────────┐ │
│  │  Hardware Abstraction Layer       │ │
│  │  (I2C, SPI, UART, WiFi, BLE)      │ │
│  └───────────────────────────────────┘ │
│  ┌───────────────────────────────────┐ │
│  │  Sensors (DHT22, MPU6050, etc.)  │ │
│  └───────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

**Data Flow:**
```
Sensor → Read → Validate → Feature Extract → Edge ML → 
Aggregate → Compress → Sign → MQTT Publish → Cloud
```

**State Machine:**
```
[INIT] → [CONNECT_WIFI] → [CONNECT_MQTT] → [READY]
                ↓                ↓              ↓
           [RETRY]          [RETRY]      [READ_SENSORS]
                                              ↓
                                         [PROCESS_DATA]
                                              ↓
                                         [PUBLISH_DATA]
                                              ↓
                                         [SLEEP/WAIT]
```

### 2.2 Backend Microservices

**Device Management Service (NestJS):**
```typescript
// Architecture
┌─────────────────────────────────────┐
│      Device Management Service      │
│  ┌───────────────────────────────┐ │
│  │     Controllers (REST API)    │ │
│  │  /devices, /telemetry, /auth  │ │
│  └───────────────────────────────┘ │
│              ↓                      │
│  ┌───────────────────────────────┐ │
│  │     Business Logic Layer      │ │
│  │  DeviceService, AuthService   │ │
│  └───────────────────────────────┘ │
│              ↓                      │
│  ┌───────────────────────────────┐ │
│  │     Data Access Layer         │ │
│  │  TypeORM Repositories         │ │
│  └───────────────────────────────┘ │
│              ↓                      │
│  ┌───────────────────────────────┐ │
│  │     PostgreSQL Database       │ │
│  └───────────────────────────────┘ │
└─────────────────────────────────────┘

// Key Entities
Device {
  id: UUID
  deviceId: string
  owner: string
  status: enum
  metadata: JSON
  createdAt: timestamp
  updatedAt: timestamp
}

Telemetry {
  id: UUID
  deviceId: FK
  timestamp: timestamp
  data: JSONB
  signature: string
}
```

**ML Inference Service (FastAPI):**
```python
# Architecture
┌─────────────────────────────────────┐
│      ML Inference Service           │
│  ┌───────────────────────────────┐ │
│  │     API Layer (FastAPI)       │ │
│  │  /predict, /batch, /health    │ │
│  └───────────────────────────────┘ │
│              ↓                      │
│  ┌───────────────────────────────┐ │
│  │     Model Manager             │ │
│  │  Load, Cache, Version         │ │
│  └───────────────────────────────┘ │
│              ↓                      │
│  ┌───────────────────────────────┐ │
│  │     Inference Engine          │ │
│  │  TorchServe / ONNX Runtime    │ │
│  └───────────────────────────────┘ │
│              ↓                      │
│  ┌───────────────────────────────┐ │
│  │     Model Storage (S3)        │ │
│  └───────────────────────────────┘ │
└─────────────────────────────────────┘

# Request Flow
Request → Validate → Preprocess → Inference → 
Postprocess → Cache → Response
```

### 2.3 Zero-Knowledge Proof System

**ZKP Service Architecture:**
```
┌─────────────────────────────────────────────┐
│         ZKP Generation Service              │
│  ┌───────────────────────────────────────┐ │
│  │     API Layer (Express)               │ │
│  │  POST /generate-proof                 │ │
│  │  POST /verify-proof                   │ │
│  └───────────────────────────────────────┘ │
│              ↓                              │
│  ┌───────────────────────────────────────┐ │
│  │     Queue Manager (Bull/Redis)        │ │
│  │  Job scheduling, prioritization       │ │
│  └───────────────────────────────────────┘ │
│              ↓                              │
│  ┌───────────────────────────────────────┐ │
│  │     Worker Pool (K8s Pods)            │ │
│  │  ┌─────────┐  ┌─────────┐            │ │
│  │  │Worker 1 │  │Worker 2 │  ...       │ │
│  │  │(GPU)    │  │(GPU)    │            │ │
│  │  └─────────┘  └─────────┘            │ │
│  └───────────────────────────────────────┘ │
│              ↓                              │
│  ┌───────────────────────────────────────┐ │
│  │     Proof Storage (IPFS)              │ │
│  │     Metadata (PostgreSQL)             │ │
│  └───────────────────────────────────────┘ │
└─────────────────────────────────────────────┘

# Proof Generation Pipeline
Input → Validate → Queue → 
Generate Witness → Prove (GPU) → 
Store IPFS → Return Hash
```

**Circuit Design:**
```
SensorAttestation Circuit:
  Inputs (Private):
    - sensorValue
    - devicePrivateKey
  Inputs (Public):
    - threshold
    - deviceId
    - timestamp
  
  Constraints:
    1. sensorValue < threshold
    2. Verify signature(devicePrivateKey, data)
    3. Compute merkleRoot(sensorValue, timestamp)
  
  Outputs (Public):
    - validReading (boolean)
    - merkleRoot (hash)
```

### 2.4 Blockchain Layer

**Smart Contract Architecture:**
```solidity
// Contract Hierarchy
┌─────────────────────────────────────┐
│      Proxy Contract (Upgradeable)   │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│      Main Logic Contract            │
│  ┌───────────────────────────────┐ │
│  │   DeviceRegistry              │ │
│  │   - registerDevice()          │ │
│  │   - deactivateDevice()        │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │   ProofVerifier               │ │
│  │   - verifyProof()             │ │
│  │   - (auto-generated)          │ │
│  └───────────────────────────────┘ │
│  ┌───────────────────────────────┐ │
│  │   DataAttestation             │ │
│  │   - attestData()              │ │
│  │   - getAttestation()          │ │
│  └───────────────────────────────┘ │
└─────────────────────────────────────┘

// Storage Layout
devices: mapping(bytes32 => Device)
attestations: mapping(bytes32 => Attestation)
proofs: mapping(bytes32 => bool)
```

**Event Indexing (The Graph):**
```graphql
# Subgraph Schema
type Device @entity {
  id: ID!
  deviceId: Bytes!
  owner: Bytes!
  isActive: Boolean!
  attestations: [Attestation!]! @derivedFrom(field: "device")
}

type Attestation @entity {
  id: ID!
  device: Device!
  dataHash: Bytes!
  proofHash: Bytes!
  timestamp: BigInt!
  blockNumber: BigInt!
}

# Query Pattern
{
  devices(where: {isActive: true}) {
    deviceId
    attestations(first: 10, orderBy: timestamp, orderDirection: desc) {
      dataHash
      timestamp
    }
  }
}
```

## 3. Data Flow Design

### 3.1 End-to-End Data Flow

```
[Device] → [MQTT Broker] → [Kafka] → [Services] → [Database]
                                ↓
                          [ML Service]
                                ↓
                          [ZKP Service]
                                ↓
                          [Blockchain]
```

**Detailed Flow:**
```
1. Device reads sensor (100ms)
2. Edge processing (50ms)
3. MQTT publish (200ms)
4. Kafka ingestion (10ms)
5. Service processing (100ms)
6. Database write (50ms)
7. ML inference (500ms)
8. ZKP generation (5-30s)
9. Blockchain attestation (2-5s)

Total: ~6-35 seconds end-to-end
```

### 3.2 Event Streaming Architecture

```
┌─────────────────────────────────────────────┐
│            Apache Kafka Cluster             │
│  ┌───────────────────────────────────────┐ │
│  │  Topics:                              │ │
│  │  - sensor-readings (10 partitions)    │ │
│  │  - ml-predictions (5 partitions)      │ │
│  │  - blockchain-events (3 partitions)   │ │
│  │  - proof-requests (5 partitions)      │ │
│  └───────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
         ↓              ↓              ↓
    [Consumer 1]   [Consumer 2]   [Consumer 3]
    Analytics      Data Lake      Real-time UI
```

## 4. Deployment Architecture

### 4.1 Kubernetes Cluster Design

```
┌─────────────────────────────────────────────────────────┐
│              AWS EKS Cluster (Production)               │
│  ┌───────────────────────────────────────────────────┐ │
│  │  Namespace: production                            │ │
│  │  ┌─────────────┐  ┌─────────────┐               │ │
│  │  │  Device API │  │   ML API    │               │ │
│  │  │  (3 pods)   │  │  (2 pods)   │               │ │
│  │  └─────────────┘  └─────────────┘               │ │
│  │  ┌─────────────┐  ┌─────────────┐               │ │
│  │  │  ZKP Worker │  │ Blockchain  │               │ │
│  │  │  (5 pods)   │  │   Service   │               │ │
│  │  └─────────────┘  └─────────────┘               │ │
│  └───────────────────────────────────────────────────┘ │
│  ┌───────────────────────────────────────────────────┐ │
│  │  Node Groups:                                     │ │
│  │  - General: t3.xlarge (3-10 nodes)               │ │
│  │  - ML: g4dn.xlarge (2-5 nodes)                   │ │
│  │  - ZKP: p3.2xlarge (2-5 nodes)                   │ │
│  └───────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

### 4.2 Network Architecture

```
┌─────────────────────────────────────────────┐
│              AWS VPC (10.0.0.0/16)          │
│  ┌───────────────────────────────────────┐ │
│  │  Public Subnet (10.0.1.0/24)          │ │
│  │  - ALB (Load Balancer)                │ │
│  │  - NAT Gateway                        │ │
│  └───────────────────────────────────────┘ │
│  ┌───────────────────────────────────────┐ │
│  │  Private Subnet (10.0.2.0/24)         │ │
│  │  - EKS Worker Nodes                   │ │
│  │  - Application Pods                   │ │
│  └───────────────────────────────────────┘ │
│  ┌───────────────────────────────────────┐ │
│  │  Database Subnet (10.0.3.0/24)        │ │
│  │  - RDS PostgreSQL                     │ │
│  │  - ElastiCache Redis                  │ │
│  └───────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
```

## 5. Security Design

### 5.1 Authentication & Authorization Flow

```
User → [Auth0] → JWT Token → [Kong Gateway] → 
[Service] → Validate Token → Process Request

Device → X.509 Cert → [AWS IoT Core] → 
[MQTT Broker] → Validate Cert → Accept Connection
```

### 5.2 Security Layers

```
Layer 1: Network Security
  - VPC isolation
  - Security groups
  - NACLs
  - WAF rules

Layer 2: Application Security
  - JWT authentication
  - API rate limiting
  - Input validation
  - CORS policies

Layer 3: Data Security
  - Encryption at rest (AES-256)
  - Encryption in transit (TLS 1.3)
  - Key rotation (90 days)
  - Secrets management (AWS KMS)

Layer 4: Monitoring
  - Intrusion detection
  - Anomaly detection
  - Audit logging
  - Compliance reporting
```

## 6. Monitoring & Observability

### 6.1 Metrics Architecture

```
┌─────────────────────────────────────────────┐
│         Prometheus (Metrics Collection)     │
│  ┌───────────────────────────────────────┐ │
│  │  Scrapers:                            │ │
│  │  - Node Exporter (system metrics)     │ │
│  │  - cAdvisor (container metrics)       │ │
│  │  - Custom exporters (app metrics)     │ │
│  └───────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│         Grafana (Visualization)             │
│  Dashboards:                                │
│  - System Health                            │
│  - API Performance                          │
│  - ML Model Metrics                         │
│  - Blockchain Activity                      │
└─────────────────────────────────────────────┘
```

### 6.2 Logging Architecture

```
Application Logs → [Fluentd] → [Elasticsearch] → [Kibana]
                                      ↓
                              [Long-term Storage (S3)]
```

### 6.3 Tracing Architecture

```
Request → [OpenTelemetry SDK] → [Jaeger Collector] → 
[Jaeger Storage] → [Jaeger UI]

Trace Example:
  API Gateway (5ms)
    → Device Service (50ms)
      → Database Query (30ms)
      → ML Service (500ms)
        → Model Inference (450ms)
    → ZKP Service (5000ms)
      → Proof Generation (4800ms)
```

## 7. Scalability Design

### 7.1 Horizontal Scaling Strategy

```
| Component          | Min | Max | Trigger           |
| ------------------ | --- | --- | ----------------- |
| Device API         | 3   | 20  | CPU > 70%         |
| ML Service         | 2   | 10  | Queue depth > 100 |
| ZKP Workers        | 2   | 15  | Queue depth > 50  |
| Blockchain Service | 2   | 5   | CPU > 80%         |
```

### 7.2 Database Scaling

```
PostgreSQL:
  - Primary (writes)
  - Read Replica 1 (reads)
  - Read Replica 2 (reads)
  - Connection pooling (PgBouncer)

TimescaleDB:
  - Hypertables for time-series
  - Continuous aggregates
  - Data retention policies
  - Compression (older data)
```

## 8. Disaster Recovery

### 8.1 Backup Strategy

```
| Component       | Frequency | Retention | Location   |
| --------------- | --------- | --------- | ---------- |
| PostgreSQL      | Daily     | 30 days   | S3         |
| TimescaleDB     | Daily     | 90 days   | S3         |
| Redis           | Hourly    | 7 days    | S3         |
| Kafka           | Real-time | 7 days    | S3         |
| Smart Contracts | On-chain  | Forever   | Blockchain |
```

### 8.2 Recovery Procedures

```
RTO (Recovery Time Objective): 4 hours
RPO (Recovery Point Objective): 1 hour

Failure Scenarios:
1. Single pod failure → Auto-restart (30s)
2. Node failure → Reschedule pods (2min)
3. AZ failure → Failover to other AZ (5min)
4. Region failure → Manual failover (4hrs)
```

## 9. Performance Targets

```
| Metric                  | Target  |
| ----------------------- | ------- |
| API Response Time (p95) | < 200ms |
| ML Inference Time (p95) | < 1s    |
| ZKP Generation Time     | < 30s   |
| Blockchain Confirmation | < 5s    |
| Device Data Latency     | < 1s    |
| Dashboard Load Time     | < 2s    |
| System Uptime           | 99.9%   |
```

## 10. Cost Optimization

### 10.1 Resource Optimization

```
Strategy:
1. Use spot instances for ZKP workers (70% savings)
2. Auto-scale down during off-peak hours
3. Use S3 lifecycle policies (move to Glacier)
4. Optimize database queries (reduce RDS size)
5. Use CloudFront CDN (reduce bandwidth)
6. Reserved instances for baseline capacity
```

### 10.2 Cost Monitoring

```
Tool: AWS Cost Explorer + Custom Dashboards

Alerts:
- Daily spend > $500
- Monthly projection > $15,000
- Unusual spike in any service
```

This system design provides a production-ready, scalable architecture using industry-standard technologies and best practices.
