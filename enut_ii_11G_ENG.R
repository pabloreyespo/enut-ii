#' enut-ii-11G-ENG
#'
#' English-renamed version of \code{enut_ii_11G}. All variable names are translated
#' to English; values and units are unchanged. See \code{enut_ii_11G} for full
#' methodological details.
#'
#' All income and expenditure variables are expressed in weekly thousands of Chilean pesos,
#' deflated by IPC adjustment (factor 0.362). Time variables are expressed in weekly hours,
#' normalized to sum to 168.
#'
#' \describe{
#'   \item{id_person}{Individual identifier}
#'   \item{id_household}{Household identifier}
#'
#'   \item{is_worker}{1 if employed, CAE = "Ocupada(o)", and positive paid work time}
#'   \item{is_family}{1 if is_worker, lives with partner, and has children in household}
#'
#'   \item{weekday}{Weekday of diary (1 = Monday to 5 = Friday)}
#'   \item{weekend_day}{Weekend day of diary (6 = Saturday, 7 = Sunday)}
#'
#'   \item{relationship_to_head}{Relationship to household head}
#'   \item{n_children_0_5}{Number of household members under age 6 (ages 0-5)}
#'   \item{n_children_6_11}{Number of household members aged 6-11}
#'   \item{n_children_0_14}{Number of household members under age 15 (ages 0-14)}
#'   \item{n_youth_12_17}{Number of household members aged 12-17}
#'   \item{n_underage}{Number of household members under 18, capped at 4}
#'   \item{n_adults}{Number of adult household members (18+), capped at 6}
#'   \item{n_time_reporters}{Number of household members who reported time use}
#'   \item{n_workers}{Number of employed workers in household}
#'   \item{n_professionals}{Number of household members with completed university or higher}
#'   \item{n_elderly}{Number of household members aged 60+}
#'   \item{has_elderly}{1 if household contains at least one elderly member aged 60+ who is not the respondent}
#'   \item{household_size}{Total household size}
#'   \item{mean_age}{Mean age of household members}
#'   \item{has_children}{1 if respondent has children living in the household}
#'   \item{in_couple}{1 if respondent is in a couple relationship (married or cohabiting)}
#'   \item{lives_with_partner}{1 if respondent lives with their partner}
#'
#'   \item{domestic_service}{1 if household receives paid domestic service}
#'   \item{help_from_relatives}{1 if household receives unpaid help from relatives or neighbours}
#'   \item{external_support}{1 if household receives any external care support}
#'
#'   \item{female}{Female indicator (1 = female, 0 = male)}
#'   \item{age}{Age in years}
#'   \item{age_bracket}{Age bracket: "12-24", "25-44", "45-65", or "66+"}
#'   \item{ses}{Socioeconomic level: 1 = Bajo, 2 = Medio, 3 = Alto}
#'   \item{education_level}{Highest completed education level: "ninguna", "primaria", "secundaria", "tecnica", or "universitaria"}
#'   \item{in_education}{1 if currently enrolled in education}
#'   \item{employed}{1 if currently employed}
#'   \item{contracted_hours}{Contracted weekly working hours (from o22c)}
#'   \item{usual_hours}{Usual weekly working hours in main occupation (from o22a)}
#'   \item{work_days_per_week}{Days worked per week in main occupation (from o22b)}
#'   \item{quintile}{Household income quintile (1-5)}
#'   \item{macrozone}{Geographic macro-zone: "norte", "metropolitana", "centro", or "sur"}
#'   \item{region}{Ordinal region code}
#'   \item{region_name}{Region name label}
#'   \item{income_share}{Respondent's share of total personal income in the household}
#'
#'   \item{employment_status}{Economic activity category: "Ocupada(o)", "Desocupada(o)", "Inactiva(o)", "Menor de 15 anos", or "Sin clasificar"}
#'   \item{formality}{Occupation formality: 1 = Formal, 2 = Informal}
#'   \item{cise}{Employment status per CISE-2018 classification}
#'   \item{occupation_group}{Occupation group per CIUO-08 grouped classification}
#'
#'   \item{swb1}{Life satisfaction. Ordinal 1-5: 1 = Totally dissatisfied, 5 = Totally satisfied. Missing: 96.}
#'   \item{swb2}{Satisfaction with distribution of domestic tasks in household. Ordinal 1-5. 85 = N/A. Missing: 96.}
#'   \item{swb3}{Satisfaction with distribution of care tasks in household. Ordinal 1-5. 85 = N/A. Missing: 96.}
#'   \item{swb4}{Perceived time adequacy for paid work. Ordinal 1-3: 1 = Not enough time, 2 = Just enough, 3 = More than enough. 85 = N/A. Missing: 96.}
#'   \item{swb5}{Perceived time adequacy for study/classes. Same scale as swb4. 85 = N/A. Missing: 96.}
#'   \item{swb6}{Perceived time adequacy for domestic work. Same scale as swb4. 85 = N/A. Missing: 96.}
#'   \item{swb7}{Perceived time adequacy for caring for household members. Same scale as swb4. 85 = N/A. Missing: 96.}
#'   \item{swb8}{Perceived time adequacy for personal care. Ordinal 1-3 (no N/A). Missing: 96.}
#'   \item{swb9}{Perceived time adequacy for leisure and social life. Ordinal 1-3 (no N/A). Missing: 96.}
#'   \item{swb10}{Self-assessed domestic work share relative to what is fair. Ordinal 1-3: 1 = Less than fair share, 2 = Fair share, 3 = More than fair share. 85 = N/A. Missing: 96.}
#'   \item{swb11}{Self-assessed care work share relative to what is fair. Same scale as swb10. 85 = N/A. Missing: 96.}
#'   \item{swb12}{Time scarcity from caring for a PSDF person. Ordinal 1-5: 1 = Never, 5 = Always. Missing: 88, 96, 99. N valid: 1378.}
#'   \item{swb13}{Caregiver burden from reconciling PSDF care with family/work. Same scale as swb12. N valid: 1378.}
#'   \item{swb14}{Negative effect of PSDF care on relationships. Same scale as swb12. N valid: 1378.}
#'   \item{swb15}{Perceived health deterioration from caring for a PSDF person. Same scale as swb12. N valid: 1378.}
#'   \item{swb16}{General sense of overload from caring for a PSDF person. Same scale as swb12. N valid: 1378.}
#'   \item{swb17}{Time scarcity from caring for a child (NNA). Same scale as swb12. N valid: 7071.}
#'   \item{swb18}{Caregiver burden from reconciling NNA care with work. Same scale as swb12. N valid: 5162.}
#'   \item{swb19}{Negative effect of NNA care on physical or mental health. Same scale as swb12. N valid: 7071.}
#'
#'   \item{inc_main_job}{Income from main occupation (weekly, thousands CLP)}
#'   \item{inc_labor}{Total labor income (weekly, thousands CLP)}
#'   \item{inc_pension}{Pension and AFP income (weekly, thousands CLP)}
#'   \item{inc_group}{Imputed group income component from household total (weekly, thousands CLP)}
#'   \item{inc_household_total}{Total household income (weekly, thousands CLP)}
#'   \item{inc_household_pc}{Per capita household income (weekly, thousands CLP)}
#'   \item{inc_group_personal}{Individual share of group income (weekly, thousands CLP)}
#'   \item{inc_personal}{Personal income: inc_labor + inc_pension + inc_group_personal (weekly, thousands CLP)}
#'   \item{household_income}{Total household disposable income (weekly, thousands CLP)}
#'   \item{inc_person_week}{Household income divided by number of members (weekly, thousands CLP)}
#'
#'   \item{t_paid_work}{Paid work, weekly hours}
#'   \item{t_domestic_work}{Unpaid domestic work (sum of all t_domestic_* categories), weekly hours}
#'   \item{t_care_work}{Unpaid care work (sum of t_care_essential + t_care_education_related + t_care_other), weekly hours}
#'   \item{t_unpaid_voluntary}{Voluntary and community work, weekly hours}
#'   \item{t_education}{Education and training, weekly hours}
#'   \item{t_leisure}{Leisure (social activities, hobbies, all media), weekly hours}
#'   \item{t_personal_care}{Personal care excluding sleep and meals, weekly hours}
#'   \item{t_meals}{Eating and drinking, weekly hours}
#'   \item{t_sleep}{Sleep; adjusted to ensure rows sum to 168, weekly hours}
#'   \item{t_commute1}{Primary commuting: travel to/from work, education, or health services, weekly hours}
#'   \item{t_commute2}{Secondary commuting: travel for household errands and caregiving, weekly hours}
#'
#'   \item{t_total}{Total weekly hours across all activities (should equal 168)}
#'   \item{wage}{Hourly wage rate: inc_labor / t_paid_work (thousands CLP per hour)}
#'
#'   \item{food}{Imputed food expenditure (individual share, weekly thousands CLP)}
#'   \item{clothing}{Imputed clothing expenditure (individual share, weekly thousands CLP)}
#'   \item{utilities}{Imputed utility and fixed household expenditure (individual share, weekly thousands CLP)}
#'   \item{household_goods}{Imputed household goods and services expenditure (individual share, weekly thousands CLP)}
#'   \item{health}{Imputed health expenditure (individual share, weekly thousands CLP)}
#'   \item{transportation}{Imputed transportation expenditure (individual share, weekly thousands CLP)}
#'   \item{communications}{Imputed communications expenditure (individual share, weekly thousands CLP)}
#'   \item{recreation}{Imputed recreation and culture expenditure (individual share, weekly thousands CLP)}
#'   \item{education_exp}{Imputed education expenditure (individual share, weekly thousands CLP)}
#'   \item{restaurants}{Imputed restaurants and food away from home expenditure (individual share, weekly thousands CLP)}
#'   \item{savings}{Imputed household savings, allocated by income share (weekly thousands CLP)}
#'   \item{total_expenses}{Total imputed expenditure: inc_personal - savings (weekly thousands CLP)}
#' }
#'
#' @seealso \code{enut_ii_11G} for Spanish variable names and full methodological details.
#'
#' @source <https://www.ine.gob.cl/enut>
#' @source <https://www.ine.gob.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares>
#'
#' @docType data
#' @keywords datasets
#' @name enut_ii_11G_ENG
#' @usage data(enut_ii_11G_ENG)
#' @format A data frame with approximately 4,000-5,000 rows and 99 variables
NULL
