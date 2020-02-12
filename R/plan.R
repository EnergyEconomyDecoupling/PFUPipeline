# The workflow plan data frame outlines what you are going to do.

# You may want to set a keyboard shortcut for executing this drake plan.
# See Tools|Modify keyboard shortcuts...
# I set 
#   * command-option-control-D to trigger "Run a drake workflow" and 
#   * command-option-control-V to trigger "Visualize a drake workflow".
# --- MKH, 12 Feb 2020
plan <- drake_plan(
  countries = c("GH"),
  paths = get_paths()
)
