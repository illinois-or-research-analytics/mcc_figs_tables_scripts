# rank_planted_nodes.R
# Returns a dataframe of in_degree values and their ranks
# for planted nodes compared against all agents.
#
# Arguments:
#   in_degrees     : vector of in_degree values for ALL agents
#   planted_values : vector of in_degree values for planted nodes only
#
# Usage examples:
#   rank_planted_nodes(df$in_degree, planted_df$in_degree)
#
#   rank_planted_nodes(
#     ps5r0[type=='agent', in_degree],
#     ps5r0[type=='agent' & planted_nodes_line_number != -1, in_degree]
#   )
#
# Note: planted_values must be a subset of in_degrees, otherwise
# match() will return NA. Always confirm planted nodes are included
# in the full agent vector before calling.

rank_planted_nodes <- function(in_degrees, planted_values) {
  all_ranks <- rank(-in_degrees, ties.method = "min")
  data.frame(
    value = planted_values,
    rank  = all_ranks[match(planted_values, in_degrees)]
  )
}
