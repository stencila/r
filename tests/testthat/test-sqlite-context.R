describe('SqliteContext', {

  it('can be constructed', {
    # In-memory database
    c <- SqliteContext$new()
    expect_equal(nrow(unpack(c$runCode('SELECT * FROM sqlite_master')$output)), 0)

    # On-disk database
    c <- SqliteContext$new(dir='test-dir-2')
    expect_equal(unpack(c$runCode('SELECT * FROM data')$output), data.frame(col_a=1:2, col_b=c('a','b'), stringsAsFactors = F))

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
