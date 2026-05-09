# Customer Churn Prediction — Telecom Dataset

**MSc Data Science Coursework | University of Surrey**  
**Language:** R | **Dataset:** Telecom Customer Churn (Kaggle)

---

## Overview

This project predicts customer churn for a telecom provider by benchmarking six classification models on a real-world dataset. The goal was to identify the strongest drivers of churn and determine the most accurate model for production use.

Churn prediction is a high-value business problem — retaining an existing customer costs significantly less than acquiring a new one. This analysis provides both a predictive model and actionable business insight into which customer behaviours signal highest churn risk.

---

## Models Compared

| Model | Accuracy | Precision |
|---|---|---|
| Logistic Regression | 85.75% | — |
| Decision Tree | — | — |
| Random Forest | — | — |
| Gradient Boosting | 88.45% | 89.09% |
| KNN | 87.07% | 88.5% |
| **XGBoost (500 rounds)** | **92.65%** | **93.08%** |

**XGBoost** achieved the best performance and was selected as the final model.

---

## Key Steps

**1. Data Preprocessing**
- Missing value imputation
- IQR-based outlier capping
- Label encoding for categorical variables
- Correlation analysis — removed 4 multicollinear charge features to reduce noise
- Min-max normalisation (for KNN) and feature scaling (for Logistic Regression)

**2. Class Imbalance Handling**
- Dataset had ~14% churn rate (imbalanced)
- Applied undersampling to balance classes before training

**3. Model Training & Evaluation**
- All models trained and evaluated on accuracy, precision, recall, and AUC-ROC
- XGBoost tuned over 500 boosting rounds

**4. Business Insight**
Feature importance analysis identified the strongest churn risk indicators:
- **Customer service call frequency** — high contact volume strongly predicts churn
- **International plan subscription** — international plan holders showed significantly higher churn rates

---

## How to Run

1. Clone the repository
```bash
git clone https://github.com/samienaseem/Customer_churn_prediction.git
cd Customer_churn_prediction
```

2. Open the R script in RStudio or run via terminal
```bash
Rscript Group_19_COMM053.R
```

3. Required R packages
```r
install.packages(c("caret", "xgboost", "randomForest", "ggplot2", "dplyr", "ROSE", "e1071"))
```

---

## Dataset

- **Source:** Kaggle — Telecom Customer Churn dataset
- **Features:** Contract type, tenure, monthly charges, international plan, customer service calls, and more
- **Target:** Binary churn label (Yes / No)

---

## Results Summary

XGBoost with 500 boosting rounds achieved **92.65% accuracy** and **93.08% precision** on the held-out test set, outperforming all other models. Class imbalance handling via undersampling improved minority class detection across all models.

---

## Author

**Saquib Naseem**  
MSc Data Science, University of Surrey  
[LinkedIn](https://linkedin.com/in/Saquib-Naseem) | [GitHub](https://github.com/samienaseem)
