app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$uploadFile(file = "exampleDTThreeRow.csv")
app$snapshot()
app$setInputs(transform = "click")
# Input 'transDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'transDataTable_rows_all' was set, but doesn't have an input binding.
# Input 'transDataTable_state' was set, but doesn't have an input binding.
app$snapshot()
# Input 'factorNestTable_rows_current' was set, but doesn't have an input binding.
# Input 'factorNestTable_rows_all' was set, but doesn't have an input binding.
# Input 'factorNestTable_state' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(selectedMultipleFacets = c("Class", "Item"))
# Input 'factorNestTable_rows_current' was set, but doesn't have an input binding.
# Input 'factorNestTable_rows_all' was set, but doesn't have an input binding.
# Input 'factorNestTable_state' was set, but doesn't have an input binding.
app$snapshot()
