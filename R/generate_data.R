#' Generate a consistent background population for synthetic registers
#'
#' @description
#' Creates a "master" tibble of unique individuals with stable attributes (PNR, gender, birthday, origin).
#' This tibble serves as the consistency anchor for other synthetic register functions (e.g., \code{\link{generate_bef}}),
#' ensuring that the same PNR always corresponds to the same person across different datasets,
#' and that the synthetic registers are limited to these PNRs in order to facilitate joins.
#'
#' @param years_to_generate A vector of years the simulation is intended to cover.
#'   Used to calculate the total pool size needed to simulate population turnover.
#' @param pnrs_per_year Integer. The target population size per year. Defaults to 1000.
#' @param turnover_per_year Integer. The "buffer" of extra individuals added per year to simulate churn/turnover.
#'   Defaults to 20.
#' @param seed Integer. The random seed used to ensure reproducibility of the PNR list. Defaults to 2109.
#'
#' @returns A \code{tibble} containing the stable population pool with variables:
#' \itemize{
#'   \item \code{pnr}: Random 12-digit Personal ID string.
#'   \item \code{opr_land}: Origin country code (ensures at least one of every foreign code).
#'   \item \code{koen}: Gender code.
#'   \item \code{foed_dag}: Birth date.
#' }
#'
#' @importFrom checkmate assert_number
#' @importFrom dplyr tibble sample_n mutate select
#' @export
#'
#' @examples
#' # Generate a background population pool
#' background_pop <- generate_background_pop(
#'   years_to_generate = 2015:2024,
#'   pnrs_per_year = 1000,
#'   turnover_per_year = 20
#' )
generate_background_pop <- function(years_to_generate = 2015:2024,
                                    pnrs_per_year = 1000,
                                    turnover_per_year = 20,
                                    seed = 2109) {
  # Set the seed locally for this function call
  set.seed(seed)

  # Calculate required pool size
  total_background_rows <- pnrs_per_year + (length(years_to_generate) * turnover_per_year)

  # GENERATE BACKGROUND POPULATION (time-stable Variables)
  # P.S. Yes, PNR and KOEN can change over time for a person changing legal sex, but that's beyond the current scope


  # Setup OPR_LAND vector

  opr_country_codes <- c(
    5115, 5902, 5901, 5101, 5126, 5128, 5422, 5607, 5104, 5130,
    5134, 5142, 5150, 5750, 5609, 5611, 5108, 5153, 5140, 5154, 5156,
    5158, 5778, 5752, 5164, 5170, 5120, 5776, 5180, 5174, 5182, 5122,
    5124, 5754, 5184, 5706, 5106, 5105, 5152, 5758, 5761, 5107, 5712,
    5109, 5759, 5111, 5756, 5110, 5700, 5159, 5160, 5757, 5151, 5162,
    5129, 5172, 5704, 5176, 5114, 5132, 5143, 5186, 5199, 5202, 5204,
    5281, 5207, 5294, 5213, 5277, 5276, 5215, 5278, 5279, 5525, 5272,
    5282, 5216, 5214, 5283, 5222, 5228, 5232, 5231, 5233, 5234, 5235,
    5236, 5238, 5242, 5297, 5243, 5244, 5284, 5245, 5240, 5247, 5285,
    5246, 5286, 5287, 5621, 5288, 5298, 5255, 5253, 5289, 5256, 5258,
    5259, 5262, 5260, 5264, 5266, 5292, 5293, 5268, 5269, 5296, 5295,
    5230, 5290, 5299, 5314, 5390, 5397, 5399, 5309, 5302, 5301, 5303,
    5305, 5526, 5307, 5304, 5306, 5394, 5316, 5318, 5322, 5324, 5325,
    5345, 5326, 5328, 5372, 5530, 5529, 5336, 5339, 5528, 5338, 5308,
    5342, 5348, 5352, 5353, 5354, 5346, 5356, 5358, 5364, 5366, 5527,
    5625, 5347, 5311, 5344, 5374, 5376, 5392, 5395, 5319, 5367, 5398,
    5535, 5404, 5708, 5710, 5406, 5410, 5408, 5412, 5416, 5531, 5474,
    5403, 5724, 5428, 5432, 5434, 5498, 5436, 5438, 5442, 5444, 5446,
    5716, 5448, 5720, 5452, 5454, 5456, 5533, 5458, 5457, 5459, 5414,
    5464, 5466, 5468, 5494, 5462, 5472, 5473, 5496, 5478, 5480, 5482,
    5418, 5484, 5486, 5722, 5424, 5492, 5718, 5493, 5714, 5488, 5402,
    5471, 5476, 5499, 5487, 5502, 5516, 5779, 5508, 5506, 5274, 5248,
    5310, 5514, 5534, 5623, 5522, 5505, 5273, 5275, 5532, 5435, 5599,
    5102, 5999, 5103, 5000, 5001, 5800, 5906
  )

  n_foreign <- length(opr_country_codes)
  # Ensure we don't have negative repetition if the pool is smaller than the code list

  n_danish <- total_background_rows - n_foreign

  opr_land_vector <- sample(c(opr_country_codes, rep(5100, n_danish)))


  # Generate Tibble
  tibble::tibble(
    # Stable ID
    pnr = replicate(total_background_rows, paste0(sample(0:9, 12, replace = TRUE), collapse = "")),

    # Stable Origin
    opr_land = opr_land_vector,

    # Stable Gender
    koen = sample(c(1, 2), total_background_rows, replace = TRUE),


    # Cap max birthday at start of sequence - 1 year to ensure valid ALDER in all generated years
    foed_dag = sample(
      seq(as.Date("1950-01-01"), as.Date(paste0(min(years_to_generate) - 1, "-12-31")), by = "day"),
      total_background_rows,
      replace = TRUE
    )
  )
}


#' Generate longitudinal synthetic BEF (Population) data
#'
#' @description
#' Generates a synthetic longitudinal dataset mimicking the Danish "BEF" (Befolkning) register.
#' It uses a pre-generated background population to ensure consistency of stable variables
#' (PNR, Gender, Birthday, Origin) across different synthetic registers.
#'
#' @param background_df A tibble containing the stable background population.
#'   This should be generated using \code{\link{generate_background_pop}}.
#' @param pnrs_per_year Integer. The number of unique individuals (rows) to sample for each year.
#'   Defaults to 1000.
#' @param years_to_generate A vector of integers or characters representing the years
#'   to generate data for (e.g., \code{2015:2024}). Defaults to 2015:2024.
#'
#' @returns A \code{tibble} containing the synthetic BEF register with the following variables:
#' \itemize{
#'   \item \code{PNR}: Personal identification number (stable).
#'   \item \code{KOEN}: Gender (1 = Male, 2 = Female) (stable).
#'   \item \code{FOED_DAG}: Date of birth (stable).
#'   \item \code{ALDER}: Age in years (calculated relative to year end).
#'   \item \code{REG}: Region code (time-varying).
#'   \item \code{CIVST}: Civil status code (time-varying).
#'   \item \code{OPR_LAND}: Country of origin code (stable).
#'   \item \code{year}: The reference year for the observation.
#' }
#'
#' @export
#'
#' @examples
#' # 1. Generate the stable background population
#' background_pop <- generate_background_pop(
#'   years_to_generate = 2015:2024,
#'   pnrs_per_year = 1000,
#'   turnover_per_year = 20
#' )
#'
#' # 2. Generate the longitudinal BEF register
#' df_bef <- generate_bef(
#'   background_df = background_pop,
#'   pnrs_per_year = 1000,
#'   years_to_generate = 2015:2024
#' )
generate_bef <- function(background_df,
                         n_samples = 1000,
                         years_to_generate = 2015:2024) {
  # A HELPER FUNCTION FOR YEARLY SAMPLING
  generate_year_sample <- function(curr_year, background_df, n_samples = n_samples) {
    force(n_samples)
    year_char <- as.character(curr_year)
    civst_codes <- c("D", "E", "F", "G", "L", "O", "P", "U", "9")

    # Slice specific rows of PNRs to generate this year
    background_df |>
      dplyr::slice_sample(n = n_samples, replace = FALSE) |>
      # Add time-varying Variables (Generated at random each year)
      dplyr::mutate(
        reg = sample(
          c(81, 82, 83, 84, 85),
          dplyr::n(),
          replace = TRUE,
          prob = c(0.1, 0.22, 0.21, 0.32, 0.15)
        ),
        civst = sample(civst_codes, dplyr::n(), replace = TRUE),
        year = year_char,

        # Calculate ALDER relative to the current year
        alder = floor(lubridate::time_length(
          lubridate::interval(foed_dag, as.Date(paste0(year_char, "-12-31"))),
          unit = "years"
        ))
      ) |>
      # Reorder columns
      dplyr::select(pnr, koen, foed_dag, alder, reg, civst, opr_land, year)
  }

  # EXECUTE MAP & RETURN
  result <- purrr::map(years_to_generate, \(yr) generate_year_sample(yr, background_df, n_samples)) |>
    purrr::list_rbind()

  return(result)
}


#' Generate synthetic administrative LPR data (lpr_adm)
#'
#' @description
#' Generates a synthetic hospital admission register (LPR - Landspatientregisteret).
#' Unlike in the population register `bef`, an individual can have multiple entries
#' per year. It generates unique 'RECNUM' identifiers for each admission, which can
#' be used to link to subsequent tables (e.g., diagnoses).
#'
#' @param background_df A tibble containing the background population (source of valid PNRs).
#' @param n_samples Integer. The number of hospital admissions to simulate per year.
#' @param years_to_generate A vector of years to generate data for.
#'
#' @returns A \code{tibble} with columns:
#' \itemize{
#'   \item \code{pnr}: Person ID (sampled from background population).
#'   \item \code{recnum}: Unique 16-digit admission identifier.
#'   \item \code{d_inddto}: Date of admission.
#'   \item \code{c_spec}: 2-digit specialty code (00-99).
#'   \item \code{year}: Reference year.
#' }
#' @export
generate_lpr_adm <- function(background_df,
                             n_samples_per_year = 1000,
                             years_to_generate = 2015:2018) {
  # HELPER: Generate one year of admission data
  generate_lpr_adm_year <- function(curr_year) {
    year_char <- as.character(curr_year)

    # 1. Define Date Range for the current year
    start_date <- as.Date(paste0(year_char, "-01-01"))
    end_date <- as.Date(paste0(year_char, "-12-31"))
    days_in_year <- seq(start_date, end_date, by = "day")

    # 2. Sample PNRs (Allow replacement: one person can have multiple admissions)
    # We grab n_samples_per_year PNRs from the background pool
    pnr_pool <- background_df |>
      dplyr::slice_sample(n = n_samples_per_year, replace = TRUE) |>
      dplyr::pull(pnr)

    # 3. Generate Data
    dplyr::tibble(
      pnr = pnr_pool,

      # RECNUM: Unique 16-digit string for every admission (first eight-digit sequence is unique,
      # also vs lpr_a_kontakt - which some report may overlap in real data)
      recnum = paste0(
        sprintf("%08d", sample(0:49999999, n_samples_per_year, replace = FALSE)),
        replicate(n_samples_per_year, paste0(sample(0:9, 8, replace = TRUE), collapse = ""))
      ),

      # D_INDDTO: Random date within the year
      d_inddto = sample(days_in_year, n_samples_per_year, replace = TRUE),

      # C_SPEC: Random 2-digit specialty code (00 to 99)
      c_spec = sprintf("%02d", sample(0:99, n_samples_per_year, replace = TRUE)),
      year = year_char
    )
  }

  # EXECUTE MAP & RETURN
  result <- purrr::map(years_to_generate, generate_lpr_adm_year) |>
    purrr::list_rbind()

  return(result)
}


#' Generate synthetic LPR3 Contact data (lpr_a_kontakt)
#'
#' @description
#' Generates a synthetic hospital contact register mimicking the structure of LPR3
#' (Landspatientregisteret version 3), covering the period from 2019-01-01 through
#' the end of the specified `coverage_through_year`.
#'
#' Data is generated as a single continuous pool of random timestamps within this range.
#'
#' @param background_df A tibble containing the background population (source of valid PNRs).
#' @param n_samples Integer. The approximate number of contacts to simulate **per year**.
#'   Total rows generated will be `n_samples` * (years of coverage).
#' @param coverage_through_year Integer. The final year of data coverage (inclusive).
#'   Must be >= 2019. Defaults to 2024.
#'
#' @returns A \code{tibble} with columns:
#' \itemize{
#'   \item \code{pnr}: Person ID.
#'   \item \code{dw_ek_kontakt}: Unique 16-digit contact identifier.
#'   \item \code{kont_starttidspunkt}: Datetime (POSIXct) of the contact start.
#'   \item \code{kont_ans_hovedspec}: Medical specialty name.
#' }
#' @export
generate_lpr_a_kontakt <- function(background_df,
                                   n_samples_per_year = 1000,
                                   coverage_through_year = 2024) {
  if (coverage_through_year < 2019) {
    stop("LPR3 data (lpr_a_kontakt) can only be generated from 2019 onwards.")
  }


  # 1. Define the full time window
  # Start: 2019-01-01 00:00:00
  t_start <- as.POSIXct("2019-01-01 00:00:00")
  # End: Dec 31st of the coverage year
  t_end <- as.POSIXct(paste0(coverage_through_year, "-12-31 23:59:59"))

  # Calculate duration in seconds
  total_duration_secs <- as.numeric(difftime(t_end, t_start, units = "secs"))

  # 2. Calculate total sample size
  # We scale the annual 'n_samples' by the number of years covered
  num_years <- coverage_through_year - 2019 + 1
  total_n <- n_samples_per_year * num_years

  # 3. Define Specialties List
  specialties_list <- c(
    "Blandet medicin og kirurgi", "Intern medicin", "Geriatri", "Hepatologi",
    "Hæmatologi", "Infektionsmedicin", "Kardiologi", "Medicinsk allergologi",
    "Medicinsk endokrinologi", "Medicinsk gastroenterologi", "Medicinsk lungesygdomme",
    "Nefrologi", "Reumatologi", "Palliativ medicin", "Akut medicin",
    "Dermato-venerologi", "Neurologi", "Onkologi", "Fysiurgi", "Tropemedicin",
    "Kirurgi", "Karkirurgi", "Kirurgisk gastroenterologi", "Plastikkirurgi",
    "Thoraxkirurgi", "Urologi", "Gynækologi og obstetrik", "Sexologi",
    "Neurokirurgi", "Ortopædisk kirurgi", "Oftalmologi", "Oto-, rhino-, laryngologi",
    "Hospitalsodontologi", "Psykiatri", "Børne- og ungdomspsykiatri",
    "Klinisk biokemi", "Klinisk fysiologi og nuclearmedicin", "Klinisk immunologi",
    "Klinisk mikrobiolog", "Klinisk neurofysiologi", "Patologisk anatomi",
    "Diagnostisk radiologi", "Klinisk farmakologi", "Klinisk genetik", "Pædiatri",
    "Anæstesiologi", "Arbejdsmedicin", "Miljømedicin", "Almen medicin",
    "Samfundsmedicin", "Retsmedicin", "Fysio- og ergoterapi", "Ikke klassificeret"
  )

  # 4. Generate Data
  dplyr::tibble(
    # Sample PNRs from the background pool (with replacement)
    pnr = background_df |>
      dplyr::slice_sample(n = total_n, replace = TRUE) |>
      dplyr::pull(pnr),

    # dw_ek_kontakt: Unique 16-digit string (first eight-digit sequence is unique,
    # also vs lpr_a_kontakt - which some report may overlap in real data)
    dw_ek_kontakt = paste0(
      sprintf("%08d", sample(50000000:99999999, total_n, replace = FALSE)),
      replicate(total_n, paste0(sample(0:9, 8, replace = TRUE), collapse = ""))
    ),

    # kont_starttidspunkt: Random datetime within the full window
    # t_start + random seconds
    kont_starttidspunkt = t_start + runif(total_n, min = 0, max = total_duration_secs),

    # kont_ans_hovedspec: Random specialty
    kont_ans_hovedspec = sample(specialties_list, total_n, replace = TRUE)
  ) |> dplyr::arrange(.data$kont_starttidspunkt)
}

#' Generate synthetic ICD-10 diagnoses for a vector of contact keys
#'
#' @description
#' Helper function that expands a list of recnum/dw_ek_kontakt keys
#' into a long-format table of diagnoses. It assigns 1-4 diagnoses per key,
#' determines the type (Hoved/Bi/Henvisning), and generates random ICD-10 codes.
#'
#' @param id_vector A vector of unique identifiers (e.g., RECNUM or dw_ek_kontakt).
#' @param id_col_name String. The name to assign to the ID column in the output tibble.
#' @param type_col_name String. The name for the diagnosis type column.
#' @param code_col_name String. The name for the diagnosis code column.
#'
#' @returns A \code{tibble} with the expanded IDs, diagnosis types, and diagnosis codes.
generate_icd_10_diagnoses <- function(record_vector,
                                      output_record_col_name = "please_provide_output_record_col_name",
                                      diag_type_col_name = "diag_type",
                                      diag_code_col_name = "diag_code") {
  n_ids <- length(record_vector)
  if (n_ids == 0) {
    return(dplyr::tibble())
  }

  # 1. Sample how many diagnoses each ID gets (1 to 4)
  # Distribution: 25% = 1 diag, 50% = 2, 23% = 3, 2% = 4
  n_diags_per_id <- sample(1:4, n_ids, replace = TRUE, prob = c(0.25, 0.50, 0.23, 0.02))

  # 2. Expand the IDs to create the base table
  expanded_ids <- rep(record_vector, n_diags_per_id)
  n_rows <- length(expanded_ids)

  # 3. Assign Diagnosis Types (A, B, H)
  seq_ids <- sequence(n_diags_per_id)

  # Default to 'B' (Secondary)
  types <- rep("B", n_rows)

  # First diagnosis is always 'A' (Main)
  types[seq_ids == 1] <- "A"

  # Randomly turn 5% of 'B's into 'H's (Referral)
  is_b <- types == "B"
  if (any(is_b)) {
    h_indices <- sample(which(is_b), size = sum(is_b) * 0.05)
    types[h_indices] <- "H"
  }

  # 4. Generate ICD-10 Codes
  # Structure: "D" + [A-Z] + [0-9][0-9] + optional [0-9]
  letters <- sample(LETTERS, n_rows, replace = TRUE)
  digits2 <- sprintf("%02d", sample(0:99, n_rows, replace = TRUE))
  digit3 <- sample(0:9, n_rows, replace = TRUE)
  has_3rd <- sample(c(TRUE, FALSE), n_rows, replace = TRUE)

  codes <- paste0("D", letters, digits2)
  codes[has_3rd] <- paste0(codes[has_3rd], digit3[has_3rd])

  # 5. Return Tibble with dynamic names
  # We construct the tibble using setNames to apply the custom column names
  dplyr::tibble(
    !!rlang::sym(output_record_col_name) := expanded_ids,
    !!rlang::sym(diag_type_col_name) := types,
    !!rlang::sym(diag_code_col_name) := codes
  )
}




#' Generate synthetic LPR2 diagnosis data (lpr_diag)
#'
#' @description
#' Generates a synthetic diagnosis table linked to an existing lpr_adm table.
#' Uses \code{generate_icd_10_diagnoses} for core logic.
#' It creates a 1-to-many relationship where each admission (recnum) can have
#' one or more diagnoses.
#'
#' @param lpr_adm_data A tibble. The output from \code{generate_lpr_adm}.
#'   Must contain a 'recnum' column (lower-cased).
#'
#' @returns A \code{tibble} with columns:
#' \itemize{
#'   \item \code{recnum}: Link to the administrative table (lowercase).
#'   \item \code{c_diagtype}: 'A' (Main), 'B' (Secondary), or 'H' (Referral).
#'   \item \code{c_diag}: ICD-10 code (e.g., 'DI20', 'DA123').
#' }
#' @export
generate_lpr_diag <- function(lpr_adm_data) {
  # Validate input
  if (!"recnum" %in% names(lpr_adm_data)) {
    stop("Input dataframe must contain a 'recnum' (lowercase) column.")
  }

  # Call helper with specific column names for the old LPR format
  generate_icd_10_diagnoses(
    record_vector = lpr_adm_data$recnum,
    output_record_col_name = "recnum",
    diag_type_col_name = "c_diagtype",
    diag_code_col_name = "c_diag"
  )
}


#' Generate synthetic LPR A diagnosis data (lpr_a_diagnose)
#'
#' @description
#' Generates a synthetic diagnosis table linked to an existing lpr_a_kontakt (`lpr_a_kontakt`) table.
#' Uses \code{generate_icd_10_diagnoses} for core logic and adds the
#' 'senere_afkraeftet' variable.
#' . It creates a 1-to-many relationship where each contact
#' (`dw_ek_kontakt`) can have one or more diagnoses.
#'
#' @param lpr_kontakt_data A tibble. The output from \code{generate_lpr_a_kontakt}.
#'   Must contain a 'dw_ek_kontakt' column.
#'
#' @returns A \code{tibble} with columns:
#' \itemize{
#'   \item \code{dw_ek_kontakt}: Link to the contact table.
#'   \item \code{diag_kode_type}: 'A' (Aktionsdiagnose), 'B' (Bidiagnose), or 'H' (Henvisningsdiagnose).
#'   \item \code{diag_kode}: ICD-10 code (e.g., 'DI20', 'DA123').
#'   \item \code{senere_afkraeftet}: 'Nej' (99%) or 'Ja' (1%).
#' }
#'
#' @param lpr_kontakt_data A tibble. The output from \code{generate_lpr_a_kontakt}.
#'
#' @export
generate_lpr_a_diagnose <- function(lpr_kontakt_data) {
  if (!"dw_ek_kontakt" %in% names(lpr_kontakt_data)) {
    stop("Input dataframe must contain a 'dw_ek_kontakt' (lower-case) column.")
  }

  # 1. Generate base diagnoses using the helper
  base_df <- generate_icd_10_diagnoses(
    record_vector = lpr_kontakt_data$dw_ek_kontakt,
    output_record_col_name = "dw_ek_kontakt",
    diag_type_col_name = "diag_kode_type",
    diag_code_col_name = "diag_kode"
  )

  # 2. Add the LPR3-specific variable 'senere_afkraeftet'
  # "Nej" for 99%, "Ja" for 1%
  retracted_status <- sample(c("Nej", "Ja"), nrow(base_df), replace = TRUE, prob = c(0.99, 0.01))

  base_df |>
    dplyr::mutate(senere_afkraeftet = retracted_status)
}
