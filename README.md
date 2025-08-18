# Pharma Company Segmentation (R)

This repository contains an end‑to‑end workflow to segment pharmaceutical companies using unsupervised learning in **R**. The goal is to turn raw fundamentals into actionable **business segments** that help guide investment or product strategy.

> **Summary of results:** Using hierarchical clustering and K‑Means on **21 companies × 9 financial metrics**, we identified **3 clear segments** with **7/9/5** firms. Cross‑method agreement was **~81% (17/21)**, and K‑Means explained **~45%** of between‑company variation (between\_SS / total\_SS = **44.7%**). A 4×4 Self‑Organizing Map (SOM) was used to visualize topology and neighborhood distances.

---

## Table of Contents
1. [Project Structure](#project-structure)
2. [Dataset](#dataset)
3. [Environment](#environment)
4. [How to Run](#how-to-run)
5. [Methodology](#methodology)
6. [Key Findings](#key-findings)
7. [Visualizations](#visualizations)
8. [Business Insights & Strategy](#business-insights--strategy)
9. [Limitations](#limitations)
10. [Next Steps](#next-steps)
11. [Citations](#citations)

---

## Project Structure

```
.
├─ data/
│  └─ Pharmaceuticals.csv                # input dataset (21 rows x 14 columns)
├─ scripts/
│  └─ analysis.R                         # main R script (see snippets below)
└─ README.md
```

> If you don’t use this exact structure, just update the file paths in your R session (e.g., `read.csv("data/Pharmaceuticals.csv")`).

---

## Dataset

**Rows:** 21 companies  
**Core features used (9):** `Market_Cap`, `Beta`, `PE_Ratio`, `ROE`, `ROA`, `Asset_Turnover`, `Leverage`, `Rev_Growth`, `Net_Profit_Margin`  
**Other columns (excluded from clustering):** identifiers and metadata such as `Symbol`, `Name`, `Median_Recommendation`, `Location`, `Exchange`.

Example schema (first few):  
- `Market_Cap` (numeric, $B)  
- `Beta` (numeric)  
- `PE_Ratio` (numeric)  
- `ROE` (%)  
- `ROA` (%)  
- `Asset_Turnover`  
- `Leverage`  
- `Rev_Growth` (%)  
- `Net_Profit_Margin` (%)

---

## Environment

- **R**: 4.2.2 (aarch64-apple-darwin20)  
- **Key packages**:  
  - Base/Recommended: `stats`, `graphics`, `utils`  
  - Clustering: `cluster`  
  - SOM: `kohonen` (and its dependency `Rcpp`)

Install packages as needed:
```r
install.packages(c("cluster", "kohonen"))
```

---

## How to Run

Minimal end‑to‑end script (place as `scripts/analysis.R` or run in an R console):

```r
# 1) Load data
data <- read.csv("data/Pharmaceuticals.csv")

# 2) Select numeric features and scale
z <- data[, c("Market_Cap","Beta","PE_Ratio","ROE","ROA",
              "Asset_Turnover","Leverage","Rev_Growth","Net_Profit_Margin")]
X <- scale(z)  # z-score

# 3) Distance + hierarchical clustering (complete & average)
dist_mat <- dist(X)
hc_complete <- hclust(dist_mat, method = "complete")
hc_average  <- hclust(dist_mat, method = "average")

# 4) Choose K via elbow (WSS) and run K-Means (k = 3)
wss <- (nrow(X) - 1) * sum(apply(X, 2, var))
for (k in 2:20) wss[k] <- sum(kmeans(X, centers = k, nstart = 25)$withinss)

set.seed(123)
km <- kmeans(X, centers = 3, nstart = 50)

# 5) Cross-method agreement (complete vs average)
m_complete <- cutree(hc_complete, 3)
m_average  <- cutree(hc_average,  3)
agreement  <- mean(m_complete == m_average)  # ~0.81 on this data

# 6) Summary
list(
  kmeans_sizes = km$size,                           # 7, 9, 5 (order may vary)
  between_ratio = km$betweenss / km$totss,         # ~0.447
  cross_method_agreement = agreement
)
```

(Optional) Self‑Organizing Map visualization:
```r
library(kohonen)
set.seed(222)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular")
map <- som(X, grid = g, alpha = c(0.05, 0.01), radius = 1)

# Basic plots
plot(map)                               # default overview
plot(map, type = "dist.neighbours")     # U-matrix (neighbour distances)
plot(map, type = "count")               # data density per node
plot(map, type = "codes")               # codebook vectors
```

---

## Methodology

1. **EDA & Preprocessing**
   - Inspected distributions and pairwise relationships.  
   - Standardized all 9 numeric features (z‑scores).

2. **Clustering**
   - **Hierarchical clustering** with Euclidean distance, complete & average linkage.
   - **K‑Means** (`k=3`), selected via qualitative elbow (WSS) and interpretability.
   - **Stability check:** Compared cluster membership between complete vs. average linkage → **~81%** agreement (17/21 firms).

3. **Evaluation**
   - K‑Means between\_SS / total\_SS = **44.7%**.  
   - Silhouette inspected qualitatively (not reported here as a single number).  
   - SOM used to inspect topological separation and neighborhood structure.

---

## Key Findings

**K‑Means (k=3) cluster sizes:** **7**, **9**, **5** (order varies by seed, but stable).  
**Between-cluster variance explained:** **~44.7%**.  
**Cross-method agreement (HC complete vs average):** **~81% (17/21)**.

**Cluster profiles (means in original units, from hierarchical characterization):**

| Segment | Market Cap | Beta | P/E  | ROE (%) | ROA (%) | Asset Turnover | Leverage | Rev Growth (%) | Net Margin (%) |
|:------:|-----------:|-----:|-----:|--------:|--------:|----------------:|---------:|---------------:|---------------:|
| **A** (High-profit incumbents) | **97.11** | 0.43 | 20.95 | **35.70** | **14.95** | 0.80 | 0.33 | 10.16 | **20.17** |
| **B** (High-price / low-profit) | 26.91 | 0.64 | **55.63** | 10.10 | 4.20 | 0.70 | 0.32 | 6.99 | **5.13** |
| **C** (Smaller, high-growth, more debt) | 8.82 | 0.62 | 19.61 | 16.96 | 6.24 | 0.54 | **1.11** | **21.14** | 13.19 |

*(Dataset means for reference: Market Cap 57.65, P/E 25.46, ROE 25.80, ROA 10.51, Leverage 0.59, Rev Growth 13.37, Net Margin 15.70.)*

---

## Visualizations

Core visuals used in the analysis:
- **Pairs plot / Scatter plots** (e.g., Revenue Growth vs Net Margin with labels)  
- **Dendrograms** (complete & average linkage)  
- **Elbow plot** (WSS vs. k)  
- **Silhouette plot** (qualitative check)  
- **SOM U‑Matrix / Counts / Codes** to inspect topology

Example snippets:

```r
# Pairs plot (quick EDA)
pairs(z)

# Labeled scatter
plot(Rev_Growth ~ Net_Profit_Margin, data = data)
with(data, text(Rev_Growth ~ Net_Profit_Margin, labels = Symbol, pos = 2, cex = 0.7))

# Dendrograms
plot(hc_complete, hang = -1, labels = data$Symbol, main = "HC (Complete)")
plot(hc_average,  hang = -1, labels = data$Symbol, main = "HC (Average)")

# Elbow plot
plot(1:20, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares")

# Silhouette (hierarchical example)
library(cluster)
plot(silhouette(cutree(hc_complete, 3), dist_mat))
```

---

## Business Insights & Strategy

- **Segment A — Profitable incumbents:** Strong profitability and returns (ROE ~35.7%, margin ~20.2%), moderate growth, lower leverage.  
  **Strategy:** Core allocation / priority partners; focus on scale features and reliability.

- **Segment B — High‑price, low‑profit:** Elevated P/E (~55.6×) with weak margins (~5.1%).  
  **Strategy:** Watchlist until margin expansion; alerts around profitability milestones.

- **Segment C — Small, fast‑growing, more debt:** Higher growth (~21.1%) with higher leverage (~1.11).  
  **Strategy:** Selective pilots; monitor balance‑sheet risk; emphasize ROI tracking.

---

## Limitations

- **Small N (21)**: Findings are directional; statistical confidence is limited.  
- **Single snapshot**: No time series; clusters may shift with new data.  
- **Feature set**: Fundamental metrics only; adding pipeline/therapeutic area/region could refine segments.

---

## Next Steps

- Add **time‑series** features (TTM, YoY deltas) and reassess stability.  
- Try **Gaussian Mixture Models** and **PAM**; report silhouette and ARI.  
- Enrich with **qualitative variables** (therapeutic focus, R&D spend, patent cliffs).  
- Package the workflow as an **R Markdown** report for reproducibility.

---

## Citations

- R Core Team (2022). **R: A Language and Environment for Statistical Computing**.  
- Wehrens, R., & Buydens, L. M. C. (2007). *Self‑ and Super‑Organizing Maps in R: The kohonen Package*. **Journal of Statistical Software**.

---

### Author
Pranav Manjunath Bhat — _Arizona State University_
