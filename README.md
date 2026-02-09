## Tourism Attractiveness across BRICS+ and G20: A Multi-Criteria Index and Its Determinants

This repository contains the data and code supporting the article “Tourism Attractiveness across BRICS+ and G20: A Multi-Criteria Index and Its Determinants.”

International tourism is a strategic economic sector, yet most existing competitiveness indicators are unilateral and fail to capture the inherently bilateral nature of tourism flows. This study bridges that gap by developing a bilateral tourism attractiveness index for 506 country pairs within the G20 and BRICS+ blocs.

The proposed index integrates three complementary dimensions:
(i) objective characteristics of tourism destinations;
(ii) measurable bilateral frictions represented by continuous indicators of geographic, cultural, linguistic, and religious distance; and
(iii) qualitative perceptions extracted from online user-generated content.

Sentiment analysis was performed on 223,212 TripAdvisor reviews using a RoBERTa transformer model to capture tourists’ subjective evaluations. These results were incorporated into the index as an additional informational layer.

The composite index was constructed using the COPRAS multi-criteria decision-making method, with attribute weights determined objectively through information entropy. This approach ensures a transparent and data-driven aggregation of heterogeneous indicators.

To identify the determinants of bilateral tourism attractiveness, the empirical strategy combined econometric and machine learning methods. A Tobit regression model was used for parametric inference, while a Random Forest algorithm with Boruta feature selection provided non-parametric validation.

The results indicate that higher GDP per capita, democratic openness, common borders, and favorable exchange rate ratios are positively associated with tourism attractiveness, while inflation and foreign direct investment show negative associations. The Random Forest model (R² = 0.863) substantially outperformed the Tobit model (pseudo-R² = 0.491), revealing significant nonlinear relationships. Convergence between methods was observed for 10 of the 19 variables analyzed, reinforcing the robustness of the findings.

This repository includes all scripts and datasets required to reproduce the index construction, sentiment analysis, and econometric and machine learning procedures, enabling full replication and further extensions of the study.
