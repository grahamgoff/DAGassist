#' Classify DAG nodes
#'
#' Labels each node as one of: `exposure`, `outcome`, `confounder`,
#' `mediator`, `collider`, `descendant_of_outcome`, or `other`.
#'
#' label definitions
#' *confounder* -- ancestor of both X and Y, and not a descendant of X
#' *mediator* -- descendant of X and ancestor of Y
#' *collider* -- node with 2 or more parents on an X / Y path (non-structural)
#' *descendant_of_outcome* -- any descendant of Y
#' `exposure` / `outcome` labeled explicitly in function call
#'
#' Notes:
#' - in definitions, x is exposure and y is outcome
#' - structural colliders are calculated, but only to define non-structural. 
#'   structural colliders are not included as a boolean flag
#' - A node may satisfy multiple properties; we also return boolean flags
#'   for each property. The `role` column gives a single "primary" label
#'   using the precedence defined below.
#'
#' @param dag A `dagitty` DAG object.
#' @param exposure Optional-- inferred from DAG if not set; character; exposure node name (X). 
#' @param outcome  Optional-- inferred from DAG if not set; character; outcome node name (Y).
#'
#' @return A data.frame with one row per node and columns:
#'   - `variable` (node name)
#'   - logical flags: `is_exposure`, `is_outcome`, `is_confounder`,
#'     `is_mediator`, `is_collider`, `is_descendant_of_outcome`,
#'     `is_descendant_of_exposure`
#'   - `role` (a single primary label)
#'
#' @examples
#'   d1 <- dagitty::dagitty("dag { Z -> X; Z -> Y; X -> Y }")   # confounder Z
#'   classify_nodes(d1, exposure = "X", outcome = "Y")
#'
#'   d2 <- dagitty::dagitty("dag { X -> M -> Y }")              # mediator M
#'   classify_nodes(d2, "X", "Y")
#'
#'   d3 <- dagitty::dagitty("dag { X -> C <- Y }")              # collider C
#'   classify_nodes(d3, "X", "Y")
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
  
  if (missing(exposure) || !nzchar(exposure)) {
    ex <- tryCatch(dagitty::exposures(dag), error = function(e) character(0))
    if (length(ex) == 1) {
      exposure <- ex
    } else {
      stop("Please supply `exposure=`; DAG has ", length(ex), " exposure(s).", call. = FALSE)
    }
  }
  if (missing(outcome) || !nzchar(outcome)) {
    out <- tryCatch(dagitty::outcomes(dag), error = function(e) character(0))
    if (length(out) == 1) {
      outcome <- out
    } else {
      stop("Please supply `outcome=`; DAG has ", length(out), " outcome(s).", call. = FALSE)
    }
  }
  
  nodes <- names(dag)
  if (!exposure %in% nodes) stop("Exposure '", exposure, "' not found in DAG.", call. = FALSE)
  if (!outcome  %in% nodes) stop("Outcome '",  outcome,  "' not found in DAG.", call. = FALSE)
  
  # ancestors and descendants of exposure and outcome, which will be useful later
  ancX  <- dagitty::ancestors(dag, exposure)
  ancY  <- dagitty::ancestors(dag, outcome)
  # without setdiff, it will set x and y as their own descendants
  descX <- setdiff(dagitty::descendants(dag, exposure), exposure)
  descY <- setdiff(dagitty::descendants(dag, outcome),  outcome)
  
  ## this part determines colliders on an x/y path. first it has to determine 
  ## neighbors and vertex chains ...
  # determine neighbors function
  neighbors_of <- function(g, v) {
    # neighbors in the skeleton = parents \union children 
    union(dagitty::parents(g, v), dagitty::children(g, v))
  }
  reachable_undirected <- function(g, start_nodes) {
    # read the whole DAG
    seen <- unique(start_nodes)
    frontier <- unique(start_nodes)
    while (length(frontier) > 0) {
      neigh <- unique(unlist(lapply(frontier, function(u) neighbors_of(g, u)), use.names = FALSE))
      next_frontier <- setdiff(neigh, seen)
      seen <- c(seen, next_frontier)
      frontier <- next_frontier
    }
    unique(seen)
  }
  ##...then it figures out which nodes have 2+ parents (structural colliders)...
  # structural colliders (2+ parents)
  parents_count <- vapply(nodes, function(v) length(dagitty::parents(dag, v)), integer(1))
  collider_structural <- nodes[parents_count >= 2]
  
  ## ...then it figures out which variables are on an x/y path (nodes that lie on 
  ## at least one path between exposure and outcome) ... 
  reachable_from_X <- reachable_undirected(dag, exposure)
  reachable_from_Y <- reachable_undirected(dag, outcome)
  on_xy_path <- intersect(reachable_from_X, reachable_from_Y)
  
  ### definition sets
  ## ...and finally, colliders are nodes with 2+ parents that lie on an x/y path
  collider_set <- intersect(collider_structural, on_xy_path) 
  conf_set <- setdiff(intersect(ancX, ancY), descX) # not a descendant of X
  med_set <- setdiff(intersect(descX, ancY), c(exposure, outcome))  # desc X/anc Y, not endpoint
  doY_set <- descY # descendants of outcome
  doX_set <- setdiff(descX, exposure) # descendants of exposure (not X itself)
  
  # assign boolean flags by set
  df <- data.frame(
    variable                   = nodes,
    is_exposure                = nodes == exposure,
    is_outcome                 = nodes == outcome,
    is_confounder              = nodes %in% conf_set,
    is_mediator                = nodes %in% med_set,
    is_collider                = nodes %in% collider_set,
    is_descendant_of_outcome   = nodes %in% doY_set,
    is_descendant_of_exposure  = nodes %in% doX_set,
    stringsAsFactors = FALSE
  )
  
  # choose primary flag by node
  # precedence is reverse-sequential
  role <- rep("other", nrow(df))
  role[df$is_confounder]             <- "confounder"
  role[df$is_mediator]               <- "mediator"
  role[df$is_descendant_of_outcome]  <- "descendant_of_outcome"
  role[df$is_collider]               <- "collider"
  role[df$is_outcome]                <- "outcome"
  role[df$is_exposure]               <- "exposure"
  
  #write to df
  df$role <- role
  # make class so we can tidy up output next
  class(df) <- c("DAGassist_roles", class(df))  
  #output
  df
}

#' Pretty-print node classifications (aligned)
#' @param x Output of classify_nodes() (class "DAGassist_roles")
#' @param n Max rows to print (default all)
#' @param ... (ignored)
#' @return Invisibly returns x
#' NOTE: will eventually need to modify this to make it latex/kable compatible,
#' but for now, just console output is fine
#' @export
print.DAGassist_roles <- function(x, n = Inf, ...) {
  df <- x
  
  # order exposure and outcome at the top every time, then everything else
  role_order <- c("confounder","mediator","collider","descendant_of_outcome","other")
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
    role     = df$role,
    X        = tick(df$is_exposure),
    Y        = tick(df$is_outcome),
    conf     = tick(df$is_confounder),
    med      = tick(df$is_mediator),
    col      = tick(df$is_collider),
    `desc(Y)`= tick(df$is_descendant_of_outcome),
    `desc(X)`= tick(df$is_descendant_of_exposure),
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

