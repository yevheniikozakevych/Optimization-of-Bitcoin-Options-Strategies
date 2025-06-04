# Bitcoin Options 0DTE Strategy Optimization

This project aims to optimize a **naked call** and **naked put** trading strategy for Bitcoin options, specifically focusing on **zero days to expiration (0DTE)** contracts.

## Project Overview

The strategy involves opening option positions **24 hours before expiration** to collect premiums. Using machine learning, the model predicts whether a Bitcoin option is likely to expire **in-the-money (ITM)** or **out-of-the-money (OTM)**. Based on these predictions and a calibrated confidence threshold, the algorithm decides whether to write a call or put option to **maximize expected returns**.

## Files

- `main_paper.R`: Main script that contains all computations and models used in the research paper.

## Full Research Paper

A full explanation of the methodology and step-by-step reasoning is available in the accompanying research paper (in Spanish):  
**[🔗 Link to the full paper]***(https://docs.google.com/document/d/1PP9ohSAK4tT4p2iu4XVwMZVc45mxPO3SnQq2qCeC2ro/edit?usp=sharing)*

## Requirements

See [`requirements.txt`](requirements.txt) for the list of R packages used in the project.

---

## Getting Started

```r
# Install required packages
install.packages(c("tidyverse", "caret", "patchwork", "corrplot", "pROC"))
```

---

## Contact

For questions or suggestions, feel free to open an issue or reach out via the repository.

---

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

