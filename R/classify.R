#' Classify DAG nodes
#'
#' Labels each node by causal role in a console tabular grid. This function is
#' mostly used as an internal helper, but can be used on its own. Users are 
#' encouraged to alternatively use `DAGassist::DAGassist(show=roles)` for role
#' table specific output.
#'
#' @note 
#' Roles legend:
#' `Exp.` = exposure 
#' `Out.` = outcome 
#' `CON` = confounder
#' `MED` = mediator
#' `COL` = collider
#' `dOut` = descendant of Out.
#' `dMed` = descendant of any mediator,
#' `dCol` = descendant of any collider 
#' `dConfOn` = descendant of a confounder on a back-door path 
#' `dConfOff` = descendant of a confounder off a back-door path 
#' `NCT` = neutral control on treatment 
#' `NCO` = neutral control on outcome 
#'
#' @param dag A `dagitty` DAG object.
#' @param exposure Optional-- inferred from DAG if not set; character; exposure node name (Exp.). 
#' @param outcome  Optional-- inferred from DAG if not set; character; outcome node name (Out.).
#'
#' @return A data.frame with one row per node and columns:
#'   - `variable` (node name)
#'   - logical flags: `is_exposure`, `is_outcome`, `is_confounder`,
#'     `is_mediator`, `is_collider`, `is_neutral_on_treatment`,
#'     `is_neutral_on_outcome`, `is_descendant_of_mediator`,
#'     `is_descendant_of_collider`, `is_descendant_of_confounder_on_bdp`,
#'     `is_descendant_of_confounder_off_bdp`
#'   - `role` (a single primary label)
#'
#' @examples
#'   d1 <- dagitty::dagitty("dag {X[exposure];Y[outcome] Z -> X; Z -> Y; X -> Y }") 
#'   classify_nodes(d1)
#'   
#' @export
classify_nodes <- function(dag, exposure, outcome) {
  # run input checks
  # we cannot run validate.R because that file diagnoses issues, rather than terminating
  # when issues are detected. i keep input checks minimal here
  if (!inherits(dag, "dagitty")) {
    stop("`dag` must be a dagitty object. Create it with dagitty::dagitty() or ggdag::dagify().",
         call. = FALSE)
  }
  
  # theoretically, i should not have to ask for exposure or outcome params because
  # i am only accepting valid ggdag objects, which should already have them specified.
  # of course, some will have multiple treatments or outcomes, in which case users
  # will need to pick one of each. 
  ## this lets users leave X and Y params empty and infer from dagitty object
  #modified to allow exposure vector, rather than forcing single treatment
  if (missing(exposure) || !length(exposure) || !nzchar(paste(exposure, collapse = ""))) {
    ex <- tryCatch(dagitty::exposures(dag), error = function(e) character(0))
    if (length(ex) == 0L) {
      stop("Please supply `exposure=`; DAG has 0 exposures.", call. = FALSE)
    }
    exposure <- ex
  }
  exposure <- as.character(exposure)
  
  if (missing(outcome) || !nzchar(outcome)) {
    out <- tryCatch(dagitty::outcomes(dag), error = function(e) character(0))
    if (length(out) != 1L) {
      stop("Please supply `outcome=`; DAG has ", length(out), " outcome(s).", call. = FALSE)
    }
    outcome <- out
  }
  nodes <- names(dag)
  missing_x <- setdiff(exposure, nodes)
  if (length(missing_x)) {
    stop("Exposure(s) not found in DAG: ", paste(missing_x, collapse = ", "), call. = FALSE)
  }
  if (!outcome %in% nodes) {
    stop("Outcome '", outcome, "' not found in DAG.", call. = FALSE)
  }
  
  # ancestors and descendants of exposure and outcome
  # aggregated over all exposures
  ancX <- unique(unlist(lapply(exposure, function(x) dagitty::ancestors(dag, x))))
  ancY <- dagitty::ancestors(dag, outcome)
  
  descX <- unique(unlist(lapply(exposure, function(x) {
    setdiff(dagitty::descendants(dag, x), x)
  })))
  descY <- setdiff(dagitty::descendants(dag, outcome), outcome)
  
  is_xy_collider <- function(m) {
    parents_m <- dagitty::parents(dag, m)
    (length(intersect(exposure, parents_m)) > 0L) && (outcome %in% parents_m)
  }
  nodes_vec <- names(dag)
  
  is_collider <- vapply(nodes_vec, is_xy_collider, logical(1))
  collider_set <- nodes_vec[is_collider]
  
  ## vector compatible definition sets
  conf_set <- setdiff(intersect(ancX, ancY), c(descX, exposure, outcome))
  med_set <- setdiff(intersect(descX, ancY), c(exposure, outcome))
  doY_set <- descY
  # descendants of mediators / colliders
  dmed_set <- character(0)
  if (length(med_set)) {
    for (m in med_set) {
      dmed_set <- union(dmed_set, setdiff(dagitty::descendants(dag, m), m))
    }
  }
  dcol_set <- character(0)
  if (length(collider_set)) {
    for (c in collider_set) {
      dcol_set <- union(dcol_set, setdiff(dagitty::descendants(dag, c), c))
    }
  }

  ##  neutral definitions 
  # neutral control on treatment: affects T, does not affect Y, not desc of X, 
  # not X/Y, not already core
  core_block <- c(
    conf_set, med_set, collider_set,
    dmed_set, dcol_set,
    descY # if it affects Y (downstream), it can't be neutral on T
  )
  neutral_trt_set <- setdiff(
    ancX,
    c(ancY, descX, exposure, outcome, core_block)
  )
  
  # neutral control on outcome: affects Y, does not affect T, not desc of X or 
  # Y, not X/Y, not already core
  neutral_out_set <- setdiff(
    ancY,
    c(ancX, descX, descY, exposure, outcome, core_block)
  )
  
  dconf_on_set  <- character(0)
  dconf_off_set <- character(0)
  if (length(conf_set)) {
    for (cz in conf_set) {
      # every directed descendant of the confounder
      desc_cz <- setdiff(dagitty::descendants(dag, cz), cz)
      if (!length(desc_cz)) next
      
      # drop exposure side / mediator side / Y itself so we don’t overwrite
      # existing roles (mediator, outcome, etc.)
      desc_cz <- setdiff(desc_cz, c(exposure, descX, outcome))
      
      # “on backdoor path” = still an ancestor of Y
      on_path  <- intersect(desc_cz, ancY)
      off_path <- setdiff(desc_cz, on_path)
      
      dconf_on_set  <- union(dconf_on_set,  on_path)
      dconf_off_set <- union(dconf_off_set, off_path)
    }
  }
  ## build df
  df <- data.frame(
    variable = nodes_vec,
    is_exposure = nodes_vec %in% exposure,
    is_outcome = nodes_vec == outcome,
    is_confounder = nodes_vec %in% conf_set,
    is_mediator = nodes_vec %in% med_set,
    is_collider = nodes_vec %in% collider_set,
    is_neutral_on_treatment = nodes_vec %in% neutral_trt_set,
    is_neutral_on_outcome   = nodes_vec %in% neutral_out_set,
    is_descendant_of_outcome = nodes_vec %in% doY_set,
    is_descendant_of_mediator = nodes_vec %in% dmed_set,
    is_descendant_of_collider = nodes_vec %in% dcol_set,
    is_descendant_of_confounder_on_bdp  = nodes_vec %in% dconf_on_set,
    is_descendant_of_confounder_off_bdp = nodes_vec %in% dconf_off_set,
    stringsAsFactors = FALSE
  )
  
  ## ensure X and Y rows are clean
  xy <- df$is_exposure | df$is_outcome
  flag_cols <- c(
    "is_confounder",
    "is_mediator",
    "is_collider",
    "is_neutral_on_treatment",
    "is_neutral_on_outcome",
    "is_descendant_of_outcome",
    "is_descendant_of_mediator",
    "is_descendant_of_collider",
    "is_descendant_of_confounder_on_bdp",
    "is_descendant_of_confounder_off_bdp"
  )
  df[xy, flag_cols] <- FALSE
  
  ## role precedence (later = higher)
  role <- rep("other", nrow(df))
  role[df$is_descendant_of_confounder_off_bdp] <- "Dconf_off"
  role[df$is_descendant_of_confounder_on_bdp]  <- "Dconf_on"
  role[df$is_neutral_on_treatment] <- "nct"
  role[df$is_neutral_on_outcome] <- "nco"
  role[df$is_confounder] <- "confounder"
  role[df$is_descendant_of_mediator] <- "Dmediator"
  role[df$is_descendant_of_collider] <- "Dcollider"
  role[df$is_mediator] <- "mediator"
  role[df$is_descendant_of_outcome] <- "dOut"
  role[df$is_collider] <- "collider"
  role[df$is_outcome] <- "outcome"
  role[df$is_exposure] <- "exposure"
  
  df$role <- role
  class(df) <- c("DAGassist_roles", class(df))
  df
}

#' Print node classifications (aligned)
#' @param x Output of classify_nodes() (class "DAGassist_roles")
#' @param n Max rows to print (default all)
#' @param ... (ignored)
#' @return Invisibly returns x
#' @export
print.DAGassist_roles <- function(x, n = Inf, ...) {
  df <- x
  
  # order exposure and outcome at the top every time, then everything else
  role_order <- c("confounder","mediator","collider",
                  "descendant_of_outcome","descendant_of_collider",
                  "descendant_of_mediator", "Dconf_on","Dconf_off",
                  "neutral_on_treatment","neutral_on_outcome", "other")
  ord <- order(
    !df$is_exposure,  # exposures first
    !df$is_outcome,   # outcomes next
    match(df$role, role_order, nomatch = length(role_order) + 1),
    df$variable
  )
  df <- df[ord, , drop = FALSE]
  
  # make a vectorized tick that prints x to mark role in a grid system
  tick <- function(z) ifelse(is.na(z), "", ifelse(z, "x", ""))
  
  # build a display table
  out <- data.frame(
    variable = df$variable,
    role = df$role,
    "Exp." = tick(df$is_exposure),
    "Out." = tick(df$is_outcome),
    conf = tick(df$is_confounder),
    med = tick(df$is_mediator),
    col = tick(df$is_collider),
    dOut = tick(df$is_descendant_of_outcome),
    dMed = tick(df$is_descendant_of_mediator),
    dCol = tick(df$is_descendant_of_collider),
    dConfOn  = tick(df$is_descendant_of_confounder_on_bdp),
    dConfOff = tick(df$is_descendant_of_confounder_off_bdp),
    NCT = tick(df$is_neutral_on_treatment),
    NCO = tick(df$is_neutral_on_outcome),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  if (is.finite(n) && nrow(out) > n) out <- out[seq_len(n), , drop = FALSE]
  
  # compute widths per column according to max chars in the list
  w_var  <- max(8, nchar("variable"), max(nchar(out$variable), na.rm = TRUE))
  w_role <- max(9, nchar("role"),     max(nchar(out$role),     na.rm = TRUE))
  flag_names <- names(out)[-(1:2)]
  w_flag <- pmax(nchar(flag_names), 1)  # each flag column width
  
  ## print header
  # variable | role | X | Y | conf | med | col| desc(Y) | desc(X)
  cat(sprintf("%-*s  %-*s", w_var, "variable", w_role, "role"))
  for (j in seq_along(flag_names)) {
    cat(sprintf("  %-*s", w_flag[j], flag_names[j]))
  }
  cat("\n") #newline
  
  # print rows
  for (i in seq_len(nrow(out))) {
    # X | exposure | x |  | x |  |  |  |  (and so on)
    cat(sprintf("%-*s  %-*s", w_var, out$variable[i], w_role, out$role[i]))
    for (j in seq_along(flag_names)) {
      val <- out[i, flag_names[j], drop = TRUE]
      cat(sprintf("  %-*s", w_flag[j], val))
    }
    cat("\n") #newline
  }
  
  invisible(x)
}

