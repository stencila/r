describe('SqliteContext', {

  it('can be constructed', {
    # In-memory database
    c <- SqliteContext$new()
    expect_equal(nrow(unpack(c$runCode('SELECT * FROM sqlite_master')$output)), 0)

    # On-disk database
    c <- SqliteContext$new(dir='test-dir-2')
    expect_equal(unpack(c$runCode('SELECT * FROM data')$output), data.frame(col_a=1:2, col_b=c('a','b'), stringsAsFactors = F))

  })

  it('has a analyseCode method', {
    c <- SqliteContext$new()

    c$runCode('CREATE TABLE "table" (TEXT col1)')

    # Not input because 'table' is a TABLE in the db
    expect_equal(c$analyseCode('SELECT * FROM table'), list(
      inputs = character(),
      output = NULL,
      value = TRUE,
      errors = NULL
    ))

    # data is not a table, max is an interpolated variable, result is output
    expect_equal(c$analyseCode('result = SELECT * FROM data WHERE x < ${max}'), list(
      inputs = c('data', 'max'),
      output = 'result',
      value = TRUE,
      errors = NULL
    ))

    # Not a select, so no value
    expect_equal(c$analyseCode('UPDATE table SET col1=1'), list(
      inputs = character(),
      output = NULL,
      value = FALSE,
      errors = NULL
    ))
  })


  it('has a executeCode method', {
    c <- SqliteContext$new()

    expect_equal(c$executeCode('SELECT 42 AS answer'), list(
      inputs = character(),
      output = NULL,
      value = pack(data.frame(answer=42)),
      errors = NULL
    ))

    expect_equal(c$executeCode('result = SELECT sum(col_a) AS sum_a FROM data WHERE col_a < ${max}', list(
      data = pack(data.frame(col_a=1:10)),
      max = pack(8)
    )), list(
      inputs = c('data', 'max'),
      output = 'result',
      value = pack(data.frame(sum_a=28)),
      errors = NULL
    ))
  })

  it('has a runCode method', {
    c <- SqliteContext$new()

    c$runCode('CREATE TABLE temp AS SELECT 1 AS col_1, 2 AS col_2')
    expect_equal(unpack(c$runCode('SELECT count(*) AS count FROM temp')$output)$count, 1)

    errors <- c$runCode('SELECT * FROM does_not_exist')$errors
    expect_equal(length(errors), 1)
    expect_equal(errors[[1]]$message, "no such table: does_not_exist")
  })

  it('has a callCode method', {
    c <- SqliteContext$new()
    data <- data.frame(col_a=1:10)

    expect_equal(
      unpack(c$callCode('SELECT sum(col_a) FROM data', inputs=list(data=pack(data)))$output)[1,1],
      sum(data$col_a)
    )

    expect_equal(
      unpack(c$callCode('SELECT sum(col_a) FROM data WHERE col_a >= ${min}', inputs=list(data=pack(data), min=pack(5)))$output)[1,1],
      sum(subset(data,col_a>=5)$col_a)
    )
  })

})
