Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Keyan Cummins
2025-10-08

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [1st Analysis (Barplots)](#1st-analysis-barplots)
  - [2nd Analysis (Generalized Linear
    Model)](#2nd-analysis-generalized-linear-model)
- [DISCUSSION](#discussion)
  - [Interpretation of 1st analysis
    (e.g. barplots)](#interpretation-of-1st-analysis-eg-barplots)
  - [Interpretation of 2nd analysis (e.g. generalized linear
    model)](#interpretation-of-2nd-analysis-eg-generalized-linear-model)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

This research focused on analyzing Salt Lake County, Utah, to determine
the host feeding habits of mosquitoes in order to find possible
amplifying hosts for the West Nile virus (WNV) and to evaluate the
possible high-risk areas for transmission. The blood meal data captured
from mosquitoes within the given collection sites were assessed for risk
WNV+ and WNV- locations, and host availability was compared for the
differing sites. The barplots showed that the blood meals from the House
Finch were most numerous overall and were disproportionately present in
WNV+ sites. A generalized linear model showed that the number of House
Finch blood meals was a strong, positive, and significant predictor of
WNV rate in a given location. These results indicate that the House
Finch is likely one of the primary WNV amplifying hosts in Salt Lake
City. To control the spread of WNV, areas with high populations of House
Finch should be the areas of focus.

# BACKGROUND

West Nile virus (WNV) is a mosquito-borne disease maintained in a
transmission cycle between mosquitoes and birds, which act as hosts for
the virus. Since being introduced to North America in 1999, WNV has
become a major public health concern, especially in the areas around
Salt Lake City. WNV is commonly found in mosquito populations in this
area. Understanding which bird species mosquitoes feed on is essential
for predicting where and how the virus spreads.

Studying mosquito feeding is crucial for predicting WNV spread. The main
focus is identifying the main carriers, or amplifying hosts. These
amplifying hosts are birds that maintain high concentrations of virus in
their blood (high viremia) long enough to infect mosquitoes that feed on
them. Research shows that species like the House Finch are highly
competent hosts because they sustain high viremia, making them important
drivers of WNV transmission \[1\].

To figure out which birds are fueling the WNV cycle in the Salt Lake
City area, we used molecular techniques. We extracted DNA from the
mosquito’s blood meal and then used Polymerase Chain Reaction (PCR) to
amplify a target gene. After PCR, we sequenced the amplified DNA and
referenced it to a database to identify the host species. Using the data
from the identified hosts and the WNV positivity rates in the local
mosquito pools allows us to understand more clearly the transmission of
WNV in these areas.

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warm-up-mosquitoes-TEMPLATE_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

Which host species is the most common amplifying host for West Nile
Virus in Salt Lake City, Utah?

## Hypothesis

The House Finch will be acting as the most common amplifying host of WNV
in Salt Lake City, Utah.

## Prediction

Locations where mosquitoes have taken blood meals from House Finches
will also exhibit higher rates of WNV-positive mosquito pools compared
to locations with less House Finch blood meals.

# METHODS

The objective was to identify host species associated with increased
West Nile virus (WNV) transmission risk in Salt Lake City. Mosquito
blood meal host data from various sites were analyzed, with each site
classified as either WNV-negative (loc_positives = 0) or WNV-positive
(loc_positives = 1).

## 1st Analysis (Barplots)

We calculated the total count of blood meals taken from each host
species, aggregating the counts separately for all WNV-negative
locations and all WNV-positive locations. This comparison was visualized
using horizontal barplots to compare host feeding patterns directly. The
barplots allowed us to visually assess if any specific host was
disproportionately targeted by mosquitoes in WNV-positive areas.

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Warm-up-mosquitoes-TEMPLATE_files/figure-gfm/first-analysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

## 2nd Analysis (Generalized Linear Model)

To formally test the relationship suggested by the barplots, we
performed a Generalized Linear Model (GLM) using the number of House
Finch blood meals as the independent variable. We ran two separate
models: one using the binary presence of WNV (loc_positives) as the
response variable (using a binomial family) and a second using the WNV
positivity rate (loc_rate) as the response variable (using a Gaussian
family). This statistical test provided a formal p-value to evaluate the
significance and strength of the House Finch as a predictor of WNV risk.

``` r
# second-analysis-or-plot, glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

## Interpretation of 1st analysis (e.g. barplots)

The first analysis, presented as a pair of barplots, summarized the
total blood meals by host species across WNV-negative and WNV-positive
collection sites. The results clearly showed that House Finch and
American Robin were the most frequent blood meal hosts overall.
Crucially, in the WNV-positive locations (WNV+), the House Finch was
overwhelmingly the dominant host, with blood meal counts noticeably
higher than any other species and higher than the counts observed in the
WNV-negative locations. This visualization strongly suggests that the
House Finch is the key host linking mosquito feeding activity to WNV
transmission in the sampled locations.

## Interpretation of 2nd analysis (e.g. generalized linear model)

The second analysis, a generalized linear model, provided statistical
confirmation of the pattern observed in the barplots. The model testing
the binary presence of WNV (loc_positives∼host_House_finch) resulted in
a significant positive coefficient for House Finch (p=0.0287). This
means that sites with more House Finch blood meals were significantly
more likely to have WNV-positive mosquito pools. Furthermore, the model
testing the WNV positivity rate (loc_rate∼host_House_finch) also showed
a highly significant positive relationship (p=4.54×10^−5). This
indicates that an increase in House Finch blood meals leads to a
statistically significant increase in the WNV infection rate at a
location. Both models support the hypothesis that House Finches are
critical drivers of WNV transmission.

However, a limitation of this study is its correlational nature. While
the strong statistical relationship suggests that the House Finch is a
key amplifying host, the analysis does not prove direct causation;
unmeasured confounding factors (such as local environmental differences
in habitat quality or microclimate) could simultaneously influence both
the mosquito feeding preference and the rate of WNV amplification.

# CONCLUSION

The overall answer to the research question is that the House Finch is
likely the most important amplifying host for West Nile virus in Salt
Lake City, Utah. The evidence robustly supports the hypothesis,
demonstrating that the abundance of House Finch blood meals is a
significant positive predictor of WNV presence and positivity rate at a
mosquito collection site. This finding is consistent with biological
data showing House Finches maintain a high, prolonged viremia \[1\],
confirming their role as a key amplifying host necessary for the virus
to spread efficiently through the mosquito population in the Salt Lake
City area. Future public health interventions should leverage this
finding by prioritizing mosquito control efforts in areas with dense
House Finch populations.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-10-08.

3.  <https://www.saltlakecounty.gov/health/news/west-nile-virus-detected-in-multiple-mosquito-samples-in-salt-lake-county/>
