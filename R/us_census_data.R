#' Retrieve Population Data by Geographical Division in the USA
#'
#' This function collects and processes U.S. population data based on geographical divisions.
#' It acquires data across various demographic, socio-economic, and education variables from the American Community Survey (ACS).
#' The output is a dataframe with geographical identifiers and proportionate distributions of the population across specified categories.
#'
#' @param geo_division A character string specifying the geographical division for which data is to be retrieved, such as states or counties.
#'
#' @return A dataframe containing the geographical identifier (GEOID), name of the geographical area (NAME), variable names,
#'         categories split from those variables, and the proportion of each category within the specified geographical division.
#'
#' @importFrom tidyr separate
#' @importFrom dplyr group_by mutate select summarise bind_rows left_join
#' @importFrom tidycensus get_acs
#'
#' @examples
#' # Assuming 'get_acs' and all transformations are defined and functional within your environment:
#' df_population = get_datagotchi_usa_population_data("california")
#'
#' @export
get_datagotchi_usa_population_data <- function(geo_division){
  variables_base <- c(ses_male.1 = "DP05_0002P",
                      ses_male.0 = "DP05_0003P",
                      ses_age_group.18_24 = "DP05_0008P",
                      ses_age_group.18_24 = "DP05_0009P",
                      ses_age_group.25_34 = "DP05_0010P",
                      ses_age_group.35_44 = "DP05_0011P",
                      ses_age_group.45_54 = "DP05_0012P",
                      ses_age_group.55_59 = "DP05_0013P",
                      ses_age_group.60_64 = "DP05_0014P",
                      ses_age_group.65_74 = "DP05_0015P",
                      ses_age_group.75_84 = "DP05_0016P",
                      ses_age_group.85_over = "DP05_0017P",
                      ses_language_en_es.english = "DP02_0113P",
                      ses_language_en_es.spanish = "DP02_0116P",
                      ses_language_en_es.spanish = "DP02_0117P",
                      ses_ethnicity.white = "DP05_0066P",
                      ses_ethnicity.black = "DP05_0067P",
                      ses_ethnicity.hispanic = "DP05_0073P",
                      ses_ethnicity.asian = "DP05_0069P",
                      ses_ethnicity.native_american = "DP05_0068P",
                      ses_ethnicity.other = "DP05_0071P",
                      ses_income_money_groups.0_9999 = "DP03_0052P",
                      ses_income_money_groups.10000_14999 = "DP03_0053P",
                      ses_income_money_groups.15000_24999 = "DP03_0054P",
                      ses_income_money_groups.25000_34999 = "DP03_0055P",
                      ses_income_money_groups.35000_49999 = "DP03_0056P",
                      ses_income_money_groups.50000_74999 = "DP03_0057P",
                      ses_income_money_groups.75000_99999 = "DP03_0058P",
                      ses_income_money_groups.100000_149999 = "DP03_0059P",
                      ses_income_money_groups.150000_199999 = "DP03_0060P",
                      ses_income_money_groups.200000p = "DP03_0061P",
                      ses_income_large_groups.low = "DP03_0052P",
                      ses_income_large_groups.low = "DP03_0053P",
                      ses_income_large_groups.low = "DP03_0054P",
                      ses_income_large_groups.low = "DP03_0055P",
                      ses_income_large_groups.low = "DP03_0056P",
                      ses_income_large_groups.mid = "DP03_0057P",
                      ses_income_large_groups.mid = "DP03_0058P",
                      ses_income_large_groups.upper_mid = "DP03_0059P",
                      ses_income_large_groups.upper_mid = "DP03_0060P",
                      ses_income_large_groups.high = "DP03_0061P",
                      ses_education.low = "DP02_0060P",
                      ses_education.low = "DP02_0061P",
                      ses_education.low = "DP02_0062P",
                      ses_education.mid = "DP02_0063P",
                      ses_education.mid = "DP02_0064P",
                      ses_education.high = "DP02_0065P",
                      ses_education.high = "DP02_0066P")
  variables_continent <- c(ses_continent_born.europe = "DP02_0106",
                           ses_continent_born.asia = "DP02_0107",
                           ses_continent_born.africa = "DP02_0108",
                           ses_continent_born.oceania = "DP02_0109",
                           ses_continent_born.latin_america = "DP02_0110",
                           ses_continent_born.north_america = "DP02_0111")
  ## variables where the variable is available for the total population
  df_base <- tidycensus::get_acs(
    geography = geo_division,
    variables = variables_base,
    year = 2022) %>%
    tidyr::separate(variable, into = c("variable", "category"), sep = "\\.") %>%
    group_by(GEOID, NAME, variable) %>%
    mutate(prop = estimate / sum(estimate)) %>%
    dplyr::select(GEOID, NAME, variable, category, prop)
  ### variables where the variable is only available by sex
  df_by_geodiv_sex_prop <- tidycensus::get_acs(
    geography = geo_division,
    variables = c(
      m = "DP02_0025P",
      f = "DP02_0031P"
    )) %>%
    group_by(GEOID) %>%
    mutate(total = sum(estimate),
           prop_sex = estimate / total) %>%
    dplyr::select(GEOID, sex = variable, prop_sex)
  df_ses_marriage <- tidycensus::get_acs(
    geography = geo_division,
    variables = c(
      ses_marriage.single.f = "DP02_0032P",
      ses_marriage.single.m = "DP02_0026P",
      ses_marriage.single.f = "DP02_0034P",
      ses_marriage.single.m = "DP02_0028P",
      ses_marriage.married_common_law.f = "DP02_0033P",
      ses_marriage.married_common_law.m = "DP02_0027P",
      ses_marriage.widowed.f = "DP02_0035P",
      ses_marriage.widowed.m = "DP02_0029P",
      ses_marriage.divorced.f = "DP02_0036P",
      ses_marriage.divorced.m = "DP02_0030P",
      ses_marriage.divorced.f = "DP02_0034P",
      ses_marriage.divorced.m = "DP02_0028P"
    )) %>%
    tidyr::separate(variable, into = c("variable", "category", "sex"), sep = "\\.") %>%
    left_join(df_by_geodiv_sex_prop, by = c("GEOID", "sex")) %>%
    mutate(estimate = estimate / 100,
           prop_state = estimate * prop_sex) %>%
    group_by(GEOID, NAME, variable, category) %>%
    summarise(prop = sum(prop_state))
  df_by_geodiv_total <- tidycensus::get_acs(
    geography = geo_division,
    variables = c(
      total = "DP02_0088"
    )) %>%
    dplyr::select(GEOID, total = estimate)
  df_continent_born <- tidycensus::get_acs(
    geography = geo_division,
    variables = variables_continent,
    year = 2022) %>%
    tidyr::separate(variable, into = c("variable", "category"), sep = "\\.") %>%
    left_join(., df_by_geodiv_total, by = "GEOID") %>%
    mutate(prop = estimate / total) %>%
    dplyr::select(GEOID, NAME, variable, category, prop)
  df <- bind_rows(df_base, df_ses_marriage, df_continent_born)
  return(df)
}
