test_that("Pipeline can be constructed", {
  op <- OutputPipeline$new()

  expect_equal(class(op)[1], "OutputPipeline")
})

test_that("OutputPipelines can have data pushed to them", {
  op <- OutputPipeline$new()
  op$push('A string')
  op$push(42, 'answer')

  expect_equal(op$pipes, list(
    'default' = '"A string"',
    'answer' = '42'
  ))
})

test_that("InputPipelines can pull data from output pipelines", {
  host$serve()

  s1 <- RSession$new()
  s1$output$push('A', 'p1')
  s2 <- RSession$new()
  s2$output$push(42, 'p2')
  s3 <- RSession$new()
  s3$output$push(3.14, 'p3')
  s3$output$push(list(a=1, b=2), 'p4')

  p <- InputPipeline$new()
  p$pipes <- list(
    p1 = s1$url,
    p2 = s2$url,
    p3 = s3$url,
    p4 = s3$url
  )

  expect_equal(p$pull('p1'), '"A"')
  expect_equal(p$str('p1'), "A")

  expect_equal(p$pull('p2'), "42")
  expect_equal(p$num('p2'), 42)

  expect_equal(p$pull('p3'), '3.14')
  expect_equal(p$num('p3'), 3.14)

  expect_equal(p$pull('p4'), '{"a":1,"b":2}')
  expect_equal(p$list('p4'), list(a=1, b=2))

})

test_that("InputPipelines can join pipes", {
  host$serve()

  # Create a bunch of pipes each starting in it's own
  # session
  pipes <- list()
  for (i in 1:10) {
    pipe <- paste0('p[',i,']')
    session <- RSession$new()
    session$output$push(i*2, pipe)
    pipes[[pipe]] <- session$url
  }

  # A new input pipelines with these pipes
  p <- InputPipeline$new()
  p$pipes <- pipes

  # Join in various ways using regex on the pipe names
  expect_equal(p$join('^p\\[2\\]$'), list('p[2]'="4"))
  expect_equal(p$join('^p\\[2|6\\]$'), list('p[2]'="4", 'p[6]'="12"))
  expect_equal(length(p$join()), 10)

})
