# llmCoder 0.1.0

* First release, built on the Neudata R package template.
* `llm_auto_code()` applies a codebook to text with any 'ellmer' chat, or
  a custom coder function. Each text is coded in its own conversation, and
  the first request is sent alone so key, credit or model errors fail fast.
* `code_agreement()` compares LLM codes with human codes: percent
  agreement, Cohen's kappa with a confidence interval, precision, recall
  and F1.
* `cross_tab_themes()` tests whether a numeric metric differs when each
  theme is present (Welch's t-test, Benjamini-Hochberg adjustment), and can
  aggregate segment-level codes to participants before joining metrics.
* `plot_theme_impact()` plots the metric difference per theme with
  confidence intervals, in Neudata colours.
* `mock_mixed_data()` and `mock_coder()` let the whole workflow run offline.
