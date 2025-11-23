# SAP MM Claim Management System (S/4HANA) 📦

### 🚀 Project Overview
A comprehensive **Claim Management System** developed on **SAP S/4HANA**.
The system automates the entire lifecycle of handling defective materials in the Supply Chain, bridging the gap between Warehouse Operations (Inventory) and Procurement (Vendor Management).

* **Role:** Team Leader & Full-stack Developer.
* **Domain:** Supply Chain Management (SCM) / Material Management (MM).
* **Scope:** 4 Key Pillars (Return, Exchange, Financial Claim, Scrapping).

---

### 📂 Code Navigation Guide
*Note: These files were extracted directly from the SAP Development Server. Please refer to this mapping to review the business logic:*

| Business Process | Main ABAP File (Search for these names) | Functionality |
| :--- | :--- | :--- |
| **1. Return Stock** | `SOURCECODEZRETURNSTOCK.abap` | Logic for creating Return POs to send defective goods back to vendors. |
| **2. Exchange Material** | `SOURCECODEZEXCHANGEMATERIAL.abap` | Handling 1-to-1 material swapping (Defective IN / New OUT). |
| **3. Claim Amount** | `SOURCECODEZCLAIMAMOUNT.abap` | Calculating total reimbursement value (Debit Memo) based on price & qty. |
| **4. GI Scrapping** | `SOURCECODEZGISCRAPPINGS.abap` | Logic for Goods Issue (Movement 551) to write off destroyed materials. |

---

### 🛠 Key Features & Technical Highlights

#### 1️⃣ Return Stock Management (Deep Logic)
* **Smart Movement Type Selection:** The system automatically determines the correct SAP Movement Type based on the business scenario:
    * **MvT 122 (Return Delivery):** Used for immediate returns referencing the original Goods Receipt (**MvT 101**).
    * **MvT 161 (Return PO):** Automates returns via "Return Purchase Order" when the goods were received long ago or require a negative quantity PO.
* **Validation:** Checks stock availability and ensures the return quantity does not exceed the original receipt.

#### 2️⃣ Material Exchange Process
* Manages the complex flow of: *Receiving defective goods -> Sending to Vendor -> Receiving replacement goods*.
* Ensures **Inventory Balance** is maintained throughout the swap process to prevent stock discrepancies.

#### 3️⃣ Financial Claim (Claim Amount)
* **Pricing Logic:** Automatically retrieves the current Material Price (Moving Average / Standard Price) from Master Data tables.
* **Calculation:** Computes the total claim value including Tax and Vendor Surcharges.
* **Output:** Generates data for **Debit Memos** to be posted in the Finance (FI) module.

#### 4️⃣ Goods Issue (GI) Scrapping
* Handles materials that cannot be returned and must be destroyed on-site.
* Triggers **Goods Movement (MvT 551 - Scrapping from unrestricted-use stock)** via BAPI in the background to adjust inventory levels automatically.

---

### 💻 Tech Stack
* **Core:** SAP S/4HANA
* **Language:** ABAP (Reports, Module Pool, BAPI, ALV OOP)
* **Database:** SAP HANA DB / SQL Logic
* **Integration:**
    * **BAPI:** `BAPI_GOODSMVT_CREATE`, `BAPI_PO_CREATE1`.
    * **Tables:** `EKKO` (PO Header), `MSEG` (Material Doc), `MARA` (Material Master).

---
© 2025 Ngo Minh Tien.
