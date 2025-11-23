# SAP MM Claim Management System (S/4HANA) 📦

### 🚀 Project Overview
This repository contains the full ABAP source code for a **Claim Management System** developed on **SAP S/4HANA**.
The system automates the workflow for handling material returns, exchanges, and financial claim calculations within the Supply Chain.

* **Role:** Team Leader & Full-stack Developer.
* **Domain:** Supply Chain Management (SCM) / Material Management (MM).

---

### 📂 Code Navigation Guide
*Note: These files were extracted directly from the SAP Development Server. Below is the guide to map files to business logic:*

| Business Logic | Main ABAP File (Look for these names) |
| :--- | :--- |
| **Claim Amount Valuation** | `SOURCECODEZCLAIMAMOUNT.abap` (Calculates total reimbursement value based on material price & quantity) |
| **Material Exchange** | `SOURCECODEZEXCHANGEMATERIAL.abap` (Main Logic for handling material exchange requests) |
| **Goods Issue Scrapping** | `SOURCECODEZGISCRAPPINGS.abap` (Logic for scrapping defective goods) |
| **Include Files** | Files ending in `...TOP`, `...F01`, `...PBO` contain global data and subroutines. |

---

### 🛠 Key Features & Technical Highlights
1.  **Financial Claim Calculation:**
    * Automatically calculates the **Claim Amount** based on current Material Master price (Moving Average/Standard Price).
    * Handles logic for **Currency Conversion** if the vendor uses a different currency.
2.  **Claim Request Automation:**
    * Validates Material Documents (GR) against Warehouse Data.
    * Logic handles: *Return Stock*, *Exchange Material*, *GI Scrapping*.
3.  **SAP Integration:**
    * Utilizes **BAPI** to post material movements (Goods Movement) automatically in the background.
    * Custom **Z-Tables** designed for tracking claim history.

### 💻 Tech Stack
* **Core:** SAP S/4HANA
* **Language:** ABAP (Reports, Module Pool, BAPI)
* **Database:** SAP HANA DB / SQL Logic

---
© 2025 Ngo Minh Tien.
