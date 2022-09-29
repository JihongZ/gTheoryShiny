app <- ShinyDriver$new("../../")
app$snapshotInit("testThreeRows")

app$uploadFile(file = "exampleDTThreeRow.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(transform = "click")
# Input 'transDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'transDataTable_rows_all' was set, but doesn't have an input binding.
# Input 'transDataTable_state' was set, but doesn't have an input binding.
# Input 'factorNestTable_rows_current' was set, but doesn't have an input binding.
# Input 'factorNestTable_rows_all' was set, but doesn't have an input binding.
# Input 'factorNestTable_state' was set, but doesn't have an input binding.
app$snapshot()
