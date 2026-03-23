# Tanzania Agricultural Price Predictions (ART Project)

## 📌 Project Overview
This repository contains the analytical pipeline for modeling and predicting rural wholesale prices for 8 Crop commodities ( maize, rice, bean, wheat, potato, bulrush millet, finger millet, and sorghum)  across **43 markets in Tanzania**.

**🔗 Interactive Report:** [View the full analysis and 2025 predictions here](https://analytics-for-rural-transformation-art.github.io/Tanzania-Agricultural-Price-Predictions/)

---

## 📂 Repository Structure

### 📁 Codes/
* **`Data_Extraction_from_PDF_To_Excel_and_cleaning.R`**: Automates data extraction from Tanzania Ministry of Industry and Trade PDF reports. Includes cleaning of  month-by-month data.
* **`LNMO-CV-Analysis.R`**: Contains the **Leave-N-Markets-Out (LNMO)** cross-validation strategy, which evaluates the	sensitivity of predictive accuracy as increasingly more markets are held out from training.
* **`Tanzania_price_predictions-notebook.Rmd`**: This is the primary workflow. Documents model fitting, evaluation, and comparison of predicted prices against regional aggregates.

### 📁 Data/
* **`Tanzania_Price_Data_AllCrops_with_Coordinates_5.csv`**: This is the master dataset. It contains cleaned, compiled, and geocoded prices for all crops from **May 2021 to January 2026**.

### 📁 Outputs/
* **Spatio-temporal Maps**: These are final model outputs for all 8 crop commodities, generated through Random Forest modeling to show real price variation across space and time.

---

## 🛠️ Methodology & Scope
* **Model:** Random Forest Regression and Autoregressive Random Forest.
* **Validation:** Temporal split, Random split, and LNMO Cross-Validation.
* **Impact:** Our framework combines periodic market reports with open spatial data (travel times, climate, population density) to provide low-cost, forward-looking price estimates. These outputs can guide targeted support measures and feed simple mobile alerts and inform  policy.

---

