# 1999 alternative treatment experiment

Run from the project root with R 4.5.1:

`Rscript SCM_Germany_Validation/Treatment_1999_Trial/run_1999.R all`

Individual suites can be selected with `07`, `08`, `09`, or `10` instead of `all`.

- 07: original eight Non-EU trade donors; fitting 1980–1998, evaluation 1999–2019. Original outcome lag blocks and nominal WDI GDP/population predictors are retained. The original model already fitted through 1998: this changes the evaluation window, not the fitting window. WDI data are fetched and cached locally, so revisions may change results relative to an old download.
- 08: original complete-case hybrid MPD/PWT pool; fitting 1970–1998, evaluation 1999–2019. Five PWT channels. Predictors use only pretreatment years; last outcome block shortened from 1994–2002 to 1994–1998.
- 09: only existing tidysynth GDP weights from `SCM_Analysis/noneu SCM result/SCM_Weights.csv`, originally estimated with treatment 1999 and preperiod 1970–1998. Six trade outcomes over 1980–2019. Taiwan is unavailable in the trade data, so its weight is excluded and remaining weights are renormalized as in the original script. The GDP size denominator is Maddison real PPP GDP, not nominal current-dollar GDP. This is a fixed-weight exercise, not a refitted trade SCM.
- 10: Germany plus exactly France, Netherlands, Austria, Spain, Finland, Portugal, Italy; PWT 10.01. Fitting 1970–1998, evaluation 1999–2019. Same five channels and outcome blocks as 08, with original native PWT size/population predictors.

The runner reuses only allowlisted data preparation assignments from each original script. Original analyses and outputs are untouched. Input panels, weights, balance, yearly gaps, summaries and fit objects are saved in this folder. In 07/08/10 all placebos and leave-one-out fits for baseline weights above 0.001 are rerun. A fit failure stops the run rather than being silently omitted. As in the originals, 07 donor placebos exclude Germany from controls, whereas 08/10 donor placebos include Germany. The reported descriptive placebo p-value is the proportion of all unit MSPE ratios (including Germany) at least as large as Germany's ratio; ties count. These differing placebo pools should be considered when comparing suites.

Plots show actual Germany black, baseline grey dashed, the alternative treatment at 1999 in red and a 2007 reference in blue. 1999 is an alternative analysis cutoff, not the historical date of WTO accession or Hartz reforms. PWT shares retain their original units and signs, including the import component convention. Trade data retain the original zero-filling convention.

Summary gaps are actual minus baseline. `Mean_Gap_1999_2007` averages 1999–2007 inclusive; `Gap_2007` is the single-year gap. Post statistics use 1999–2019 inclusive. `COMPLETE.txt` is written only after a suite completes successfully.
