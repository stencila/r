describe('SqliteContext', {
  it('can be constructed', {
    # In-memory database
    c <- SqliteContext$new()
    expect_equal(nrow(c$unpack(c$executeCode('SELECT * FROM sqlite_master')$value)), 0)

    # On-disk database
    c <- SqliteContext$new(dir='test-dir-2')
    expect_equal(
      c$unpack(c$executeCode('SELECT * FROM data')$value),
      data.frame(col_a=1:2, col_b=c('a','b'), stringsAsFactors = F)
    )
  })

  it('has a analyseCode method', {
    c <- SqliteContext$new()

    c$executeCode('CREATE TABLE "table" (TEXT col1)')

    # Not input because 'table' is a TABLE in the db
    expect_equal(c$analyseCode('SELECT * FROM table'), list(
      inputs = character(),
      output = NULL,
      value = TRUE,
      messages = NULL
    ))

    # data is not a table, max is an interpolated variable, result is output
    expect_equal(c$analyseCode('result = SELECT * FROM data WHERE x < ${max}'), list(
      inputs = c('data', 'max'),
      output = 'result',
      value = TRUE,
      messages = NULL
    ))

    # Not a select, so no value
    expect_equal(c$analyseCode('UPDATE table SET col1=1'), list(
      inputs = character(),
      output = NULL,
      value = FALSE,
      messages = NULL
    ))
  })


  it('has a executeCode method', {
    c <- SqliteContext$new()

    #expect_equal(c$executeCode('SELECT 42 AS answer'), list(
    #  inputs = character(),
    #  output = NULL,
    #  value = pack(data.frame(answer=42)),
    #  errors = NULL
    #))

    expect_equal(c$executeCode('result = SELECT sum(col_a) AS sum_a FROM data WHERE col_a < ${max}', list(
      data = c$pack(data.frame(col_a=1:10)),
      max = c$pack(8)
    )), list(
      inputs = c('data', 'max'),
      output = 'result',
      value = c$pack(data.frame(sum_a=28)),
      messages = NULL
    ))

    messages <- c$executeCode('SELECT * FROM does_not_exist')$messages
    expect_equal(length(messages), 1)
    expect_equal(messages[[1]]$message, "no such table: does_not_exist")


    data <- data.frame(col_a=1:10)

    expect_equal(
      c$unpack(c$executeCode('SELECT sum(col_a) FROM data', inputs=list(data=c$pack(data)))$value)[1,1],
      sum(data$col_a)
    )

    expect_equal(
      c$unpack(c$executeCode('SELECT sum(col_a) FROM data WHERE col_a >= ${min}', inputs=list(
        data=c$pack(data),
        min=c$pack(5)
      ))$value)[1,1],
      sum(subset(data,col_a>=5)$col_a)
    )
  })

})
