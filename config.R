# =============================================================================
# CONFIGURATION FILE FOR EFFECTIVE GIVING AI PERSUASION PROJECT
# =============================================================================
# This file contains all global variables, settings, and constants used throughout
# the analysis. It should be sourced early in the analysis pipeline.
# =============================================================================

# =============================================================================
# EXPERIMENTAL CONDITIONS
# =============================================================================
# Treatment condition labels for plots and tables

# Full condition names for detailed descriptions
cond_names <- c(
  control = "Control conversation", 
  static_treatment = "Static message", 
  conv_treatment = "Persuasive LLM conversation"
)

# Short condition names for compact displays
cond_names_short <- c(
  control = "Control", 
  static_treatment = "Static Msg", 
  conv_treatment = "LLM Conv." 
)

# Comparison names for statistical contrasts
comparison_names <- c(
  "mean(static_treatment) - mean(control)" = "Static Message - Control",
  "mean(conv_treatment) - mean(control)" = "LLM conversation - Control",
  "mean(conv_treatment) - mean(static_treatment)" = "LLM conversation - Static Message"
)

comparison_names_short <- c(
  "mean(static_treatment) - mean(control)" = "Static Message - Control",
  "mean(conv_treatment) - mean(control)" = "LLM conv. - Control",
  "mean(conv_treatment) - mean(static_treatment)" = "LLM conv. - Static Message"
)

comparison_names_cate <- paste0(comparison_names, ", CATE")
names(comparison_names_cate) <- names(comparison_names)

# =============================================================================
# VISUALIZATION SETTINGS
# =============================================================================

# AMF brand colors
amf_blue <- "#0193CF"
amf_red <- "#CB1031"

# Color palettes
locuszoom_colors <- ggsci::pal_locuszoom("default")(4)

# Contrast colors for treatment comparisons
contrast_colors <- c(
  "#EEA236FF", #static v control
  "#5CB85CFF", # llm v control
  "#46B8DAFF" #llm v static
)
names(contrast_colors) <- names(comparison_names)

# Set default ggplot theme
ggplot2::theme_set(ggplot2::theme_bw())

# Shared theme elements for consistent plot styling
shared_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8.5),
  axis.text  = element_text(size = 7),
  plot.tag.position = c(0.0, 0.95),  # top-left corner
  plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold"),
  panel.grid = element_blank()
)

# =============================================================================
# CHARITY NAME MAPPINGS
# =============================================================================
# Short names for charity display in plots and tables

short_names <- c(
  "ALSAC - St. Jude Children's Research Hospital"                                  = "St. Judes",
  "American Red Cross"                                                             = "Red Cross",
  "Feeding America"                                                                = "Feeding America",
  "Doctors Without Borders, USA"                                                   = "Doctors Without Borders",
  "American Society for the Prevention of Cruelty to Animals"                     = "ASPCA",
  "Make-A-Wish America"                                                            = "Make-A-Wish",
  "Habitat for Humanity International"                                            = "Habitat for Humanity",
  "Salvation Army World Service Office Sawso"                                      = "Salvation Army",
  "Goodwill Industries International Inc."                                        = "Goodwill",
  "Humane World for Animals (formerly known as the Humane Society of the United States)" = "Humane Society"
)

# Function to clean variable names for display
clean_names <- function(string_vec) {
  out <- gsub("_", " ", string_vec) # Replace underscores with spaces
  out <- tolower(out) # Convert to lower case
  out <- gsub("(^|[.?!]\\s*)([a-z])", "\\1\\U\\2", out, perl = TRUE) # Capitalize first letter
  out <- gsub("\\lgbtqia\\b", "LGBTQIA+", out, ignore.case = TRUE) # Replace LGTQI (any case) with LGBTQIA+
  out
}

# =============================================================================
# PERSUASIVE STRATEGY DEFINITIONS
# =============================================================================
# Detailed descriptions of each persuasive strategy used in the LLM conversations

STRATEGY_DESCRIPTIONS <- c(
  EffectivenessFraming = "Emphasizes how much good AMF can do per dollar, especially in terms of lives saved or improved, framing the comparison in terms of impact per donation.",
  CostEffectivenessGap = "Emphasizes that some interventions—such as distributing malaria nets—can be hundreds or even thousands of times more effective than average charity work.",
  GoalMatching         = "When donors mention a favorite charity's mission (e.g., helping children), reframes AMF as pursuing the same abstract goal more efficiently and at greater scale.",
  MoralReasoning       = "Uses explicit moral arguments to show that giving to the more effective charity is not just preferable but morally required, often via analogies (e.g., emergency triage), appeals to impartial obligation, or harm‐reduction logic.",
  Personalization      = "Tailors the case to the participant's own stated reasons for donating or their explanation of why their favorite charity matters.",
  SplitDonation        = "Suggests splitting the gift (e.g., 50/50 or 80/20) between the user's favorite charity and AMF as a compromise.",
  ExpandingMoralConcern= "Frames giving to AMF as extending empathy and moral responsibility beyond one's immediate community.",
  AvoidingRegret       = "Invites the donor to consider which choice they'll be proud of in hindsight.",
  SocialNorms          = "Notes that many others have faced the same choice and opted for AMF, or cites how common AMF donations are.",
  AgencyFraming        = "Emphasizes the donor's personal power and agency to make a meaningful difference.",
  MoralConsistency     = "Encourages acting in line with one's values (e.g., 'You care deeply about helping others—AMF lets you live that value in the most effective way possible.').",
  EfficiencyAndScale   = "Highlights how AMF leverages bulk purchasing, low overhead, and durable nets to stretch every dollar and distribute at large scale with minimal leakage.",
  Transparency         = "Points out AMF's detailed public reports, third‐party audits, and opportunities for donors to verify outcomes firsthand.",
  IndependentEndorsements = "Cites GiveWell's top rating and/or similar external evaluations as proof of AMF's rigor and impact.",
  LegitimizingSmallContributions = "Normalizes minimal gifts by stressing that 'even a penny will help.'",
  Observability        = "Stresses that others can—or may—see the donor's charitable actions.",
  IdentifiableVictim   = "Focuses on individual beneficiaries rather than abstract statistics to make the need more tangible.",
  PromotingDeliberation= "Encourages thoughtful reflection rather than snap‐judgment or gut responses.",
  PiquePricing         = "Requests unusual donation amounts (e.g., 17¢ instead of standard denominations) to capture attention.",
  GainFramedMessaging  = "Frames outcomes in terms of positive gains (e.g., 'save a child') rather than losses (e.g., 'a child will go hungry').",
  PerceivedNeed        = "Emphasizes the urgency or severity of beneficiaries' needs to spur action.",
  WarmGlow             = "Highlights the donor's internal emotional reward (e.g., 'you'll feel good') as a motivator.",
  SocialIdentity       = "Tailors language to donors' social identities (e.g., 'As parents, we know how important a safe home is…').",
  VirtueLabeling       = "Labels the donor or the act of giving as generous, heroic, or kind to reinforce their moral identity.",
  GuiltAppeals         = "Induces a moderate sense of guilt (e.g., 'If we turn a blind eye, who will help?' or 'For the price of a coffee you could save a life—doing nothing costs more.')."
)

# Short descriptions for compact displays (e.g., plot labels)
SHORT_STRAT_DESCRIPTIONS <- c(
  EffectivenessFraming       = "Highlights lives saved or improved per dollar donated",
  CostEffectivenessGap       = "Emphasizes effectiveness gap between average and effective charities",
  GoalMatching               = "Reframes AMF as pursuing participants goals, but more efficiently",
  MoralReasoning             = "Explicit moral arguments that giving effectively is morally required",
  Personalization            = "Adapts responses to each donor’s reasons or values",
  SplitDonation              = "Suggests a split (e.g. 80/20) between AMF and favorite charity",
  ExpandingMoralConcern      = "Frames AMF giving as empathy beyond one’s immediate community",
  AvoidingRegret             = "Invites consideration of which choice you’ll feel proudest about later",
  SocialNorms                = "Cites others’ choices or donation norms to nudge toward AMF",
  AgencyFraming              = "Emphasizes ones personal power to make a meaningful difference",
  MoralConsistency           = "Encourages alignment between ones values and donation choice",
  EfficiencyAndScale         = "Points out AMF’s bulk-buying and low overhead to maximize reach",
  Transparency               = "Highlights AMF’s detailed audits, reports, and donor verification",
  IndependentEndorsements     = "Cites GiveWell’s top rating and similar external evaluations",
  LegitimizingSmallContributions = "Normalizes even tiny gifts (e.g. even a penny helps)",
  Observability              = "Notes that ones charitable action may be visible to others",
  IdentifiableVictim         = "Puts a human face on need by focusing on individual beneficiaries",
  PromotingDeliberation      = "Encourages thoughtful reflection rather than snap-decision making",
  PiquePricing               = "Uses odd amounts (e.g. 17¢) to capture donor attention",
  GainFramedMessaging        = "Frames impact in terms of positive gains (e.g. lives saved)",
  PerceivedNeed              = "Stresses urgency or severity of need to spur immediate action",
  WarmGlow                   = "Highlights the internal emotional reward of giving to AMF",
  SocialIdentity             = "Tailors messaging to donors’ own group or role identities",
  VirtueLabeling             = "Labels donors as generous or heroic to reinforce self-image",
  GuiltAppeals               = "Invokes guilt to motivate giving to AMF"
)

# Strategy names vector for easy access
strat_names <- names(SHORT_STRAT_DESCRIPTIONS)

# =============================================================================
# MOTIVATION CATEGORIES
# =============================================================================
# Categories used to classify participants' stated motivations for giving

MOTIVATION_DESCRIPTIONS <- c(
  AwarenessOfNeed    = "Mentioning or implying awareness of others' needs or suffering.",
  Solicitation       = "Reference to being asked or prompted to give.",
  CostsAndBenefits   = "Discussion of affordability, financial trade-offs, or incentives.",
  Altruism           = "Indications of selfless concern for others without expectation of return.",
  Reputation         = "Considerations of social recognition, image, or being seen as generous.",
  PsychologicalBenefits = "Emotional rewards like feeling good, avoiding guilt, or personal satisfaction.",
  Values             = "Expressions of personal, moral, religious, or political beliefs that support giving.",
  Efficacy           = "Beliefs about whether the donation makes a real impact or is effectively used."
)

# Motivation names vector for easy access
motivations <- names(MOTIVATION_DESCRIPTIONS)

# =============================================================================
# OUTPUT SETTINGS
# =============================================================================
# Settings for figure and table outputs

# Figure dimensions (in inches)
FIGURE_WIDTH <- 7.085  # Maximum width for journal submission
FIGURE_DPI <- 300
