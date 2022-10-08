app <- ShinyDriver$new("../../")
app$snapshotInit("RunGtheoryBootstrap")

app$uploadFile(file = "Brennan.3.2Wide.csv")
# Input 'rawDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'rawDataTable_rows_all' was set, but doesn't have an input binding.
# Input 'rawDataTable_state' was set, but doesn't have an input binding.
app$setInputs(transform = "click")
# Input 'transDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'transDataTable_rows_all' was set, but doesn't have an input binding.
# Input 'transDataTable_state' was set, but doesn't have an input binding.
app$snapshot()
# Input 'nestedStrucTable_rows_current' was set, but doesn't have an input binding.
# Input 'nestedStrucTable_rows_all' was set, but doesn't have an input binding.
# Input 'nestedStrucTable_state' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(runRecommModel = "click")
app$setInputs(runRecommModelBoot = "click")
app$snapshot()
