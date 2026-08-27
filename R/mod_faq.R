#' FAQ module UI
#'
#' Static content only -- no server logic.
#'
#' @param id a unique identifier for this module.
#'
#' @return a `tagList` of UI elements
#' @export
mod_faq_ui <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Does the CLIAR Benchmarking collect new data on governance and institutions?",
      p(
        "No.
        The CLIAR Benchmarking collects indicators that are publicly available and have been validated by our internal review process as proxies to measure country-level governance and institutions, with their corresponding caveats and limitations.
        In some exceptional cases, CLIAR does combine existing indicators to create new ones (e.g., aggregation of binary indicators); these are detailed in the CLIAR Methodological Note."
      )
    ),


    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Can I add my own indicators to the dashboard and run the analysis including these indicators? ",
      p(
        "You cannot add indicators to the dashboard.
        However, you can download the full database and augment it with additional indicators to customize your analysis.
        You can also get in touch with the CLIAR team (CLIAR@worldbank.org) indicating which data you would like to be added in the database, and for which cluster.
        Each request will be reviewed by a team of technical experts and if the indicator meets the selection criteria indicated in the methodological note (quality and coverage) it will be added during the next update round."
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "What does the traffic coloring mean? Is there a methodological foundation?",
      p(
        "The results from the institutional benchmarking are relative for
        a given country of interest vis a vis a chosen set of comparator countries.
        Using the distribution of the CTF scores in the set of comparator countries,
        we identify the score range for the bottom 25% of comparators,
        the score range for the 25%-50% group and the score range for the top 50% of comparators (or alternatively, using 33% and 66% as thresholds).
        Given the CTF score of the country of interest,
        we identify whether the country of interest for the analysis belong to the bottom,
        middle or top group. These percentile groups are used because they are simple and intuitive."
      )
    ),
    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Why the length of the bar is different? Why a red bar is longer than another red bar, if they are both red?",
      p(
        "Using the distribution of the CTF scores in the set of comparator countries,
        we identify the score range for the bottom 25% of comparators,
        the score range for the 25%-50% group and the score range for the top 50% of comparators.
        The red bar represents the score range for the bottom 25% of comparators. (The same explanation applies if 33% and 66% thresholds are used.)
        While the CTF scores always range between 0 and 1,
        the length of the red bar varies across indicators depending on the distribution of the CTF scores in the comparator group.
        As an illustration, for a given set of comparator countries,
        for a given indicator the CTF scores in the bottom 25% of comparators may range between 0 and 0.2,
        while for another indicator it may range between 0 and 0.5."
      )
    ),
    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "What is the difference between the static and dynamic benchmarking?",
      p(
        "The static benchmarking presents a cross-country snapshot based on averaging available indicators over the period 2018-2022; CTF calculations and distributional analysis are implemented over that cross-section. The dynamic benchmarking
        , computes CTF scores at the individual level on an annual basis, from 2013 to 2022, and distributional analysis is implemented on an annual basis, when data is available. Given data limitations, the dynamic benchmarking is more limited in the number of indicators and families analyzed --and some families are not included precisely because they do not offer data that could be aggregated and compared over time."
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Why are certain indicators or institutional families not appearing in my benchmarking results?",
      p(

        "Indicators that are missing for the base country or exhibit low variance are dropped from the analysis. In some cases, such as for the SOE Governance family, this can result in dropping an entire institutional cluster."
    )),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Can I change in the dashboard the time period over which the benchmarking is applied?",
      p(
        "The Dashboard does not offer that functionality, but such customized analyzed could be performed by downloading the data from the dashboard."
      )
    ), box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Why are certain indicators and clusters not included in the dynamic benchmarking?",
      p(
        "Compared to static benchmarking, dynamic benchmarking is more selective (or \u201cdemanding\u201d) with respect to indicators, considering their panel characteristics. Hence, indicators that do not offer multiple measurements for the same country are excluded from the analysis \u2013 e.g., OECD PMR and PEFA, which consequently excludes the SOE Governance Institutions and Public Finance Institutions indicator clusters from dynamic benchmarking"
      )
    ),


    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "How do you deal with missing data for certain indicators and for certain countries?",
      p(
        "We deal with missing data in various ways.
        First, the (static) benchmarking analysis uses the average of indicators over recent years.
        Conceptually, governance and institutional indicators are expected to show limited yearly variations.
        This helps in reducing data gaps.
        Second, we only include in the institutional benchmarking the indicators that
        are non-missing for the country of interest.
        Third, we only include in the institutional benchmarking the indicators
        that are non-missing for at least 70% of the countries in the comparator group.
        The average CTF scores at institutional cluster level are calculated
        as averages of the CTF scores of the indicators in that clusters,
        but only for the indicators that meet these criteria above.
        This ensures that,
        for each pair of country of interest and group of comparator countries,
        the average CTF scores are calculated from the same indicators."
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Why do I have to choose at least 10 comparator countries for the benchmarking analysis?",
      p(
        "The percentile analysis identifies whether the performance of the
        country of interest in a given indicator or institutional cluster
        belongs to the bottom 25%, the 25%-50% group or the top 50% of
        the comparator countries (or, alternatively, the groups based on 33% and 66% thresholds).
        This percentile analysis can be meaningfully performed only if
        there is sufficient number of comparator countries."
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "How do you choose the comparator countries/groups?",
      p(
        "It depends on the purpose of the analysis and the country context.
        For example, many reports have used regional,
        aspirational, and structural peers as identified by World Bank Country Teams."
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Can I download the raw data for my own research/analytical purposes?",
      p(
        'Yes, the full compiled database is available in the "Data" tab for download.
        Both the "Closeness to Frontier" scores and the full database with yearly indicators are available for download.'
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Why are certain cluster averages missing when I download the data even if there is non-missing data on the indicators of that cluster?",
      p(
        "A balanced sample of individual CTF scores is aggregated by cluster to create cluster-level CTF scores. For each institutional cluster, a \u201cbalanced\u201d subset of countries with full coverage (i.e., non-missing data) across all indicators within each cluster is created. This ensures that each cluster-level aggregate score relies on the same set of indicators for every country, allowing for robust and methodically sound inferences. The CTF cluster-level score is computed via simple averaging of the indicators within each cluster. This cluster-level score captures the overall performance for a given institutional category relative to the \u201cglobal frontier.\u201d The drawback of this robust methodological aggregation decision is that the data requirement is higher. Several families in both the static and dynamic versions do not meet the data requirements for meaningful aggregation (i.e., the balanced sample is too small or empty), and thus CTF cluster scores are not computed."
      )
    ),


    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "How often is the CLIAR data updated? How do I know that the CLIAR data uses the latest available data?",
      p(
        'It is currently planned that the CLIAR Database will be updated once per year.
        The CLIAR Benchmarking Dashboard is programmed so that the data extraction from the data sources (primarily EFI360)
        is automated through APIs,
        therefore with minimal maintenance costs for the indicators already
        included in the dashboard and with stable APIs. The full compiled database,
        once updated, is available in the "Data" tab for download.
        Both the CLIAR Benchmarking "Closeness to Frontier" scores and the full CLIAR master database with
        yearly indicators are available for download and therefore users can easily verify the latest year available for each indicator.'
      )
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "How were the indicators included in the CLIAR Benchmarking selected?",
      p(
        'The indicators included in the CLIAR Benchmarking were selected following a criteria of
        (geographical and time) coverage and quality.
        This list was defined based on initial internal reviews,
        and will be further refined based on inputs recently received by
        sector experts and from the experiences of country teams in applying this tool.
        The list of indicators used will be periodically reviewed in order to include new indicators that may be become available in the future.
        As such, the CLIAR database is a "live tool".'
      )
    ),
    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "How does CLIAR manage changes in the methodology of the construction of individual indicators used in the CLIAR database?",
      p(
        'CLIAR aims to keep consistent indicators. Hence, if specific indicators go through changes in their methodology, CLIAR will keep only those that are consistent, prioritizing the most recent ones. Some examples include PEFA and PMR indicators. If such change means a given indicator no longer meets the benchmarking criteria, then it is dropped from the benchmarking analysis.'
      )
    )
  )
}
