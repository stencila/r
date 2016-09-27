test_that("Component an be constructed", {
  c = Component$new()

  expect_equal(class(c)[1], "Component")
})

test_that('Component can be read and written', {
  p1 = paste0(tempfile(), '.tmp')
  p2 = paste0(tempfile(), '.tmp')
  # R doesn't actually creates the files, so do that
  cat('', file = p1)
  cat('', file = p2)

  c = Component$new(p1)
  expect_equal(c$path, p1, 'Initial path is set')

  c$read()
  expect_equal(c$path, p1, 'Read with no arg does not change path')

  c$read(p2)
  expect_equal(c$path, p2, 'Read with arg does change path')

  c$write()
  expect_equal(c$path, p2, 'Write with no arg does not change path')

  c$write(p1)
  expect_equal(c$path, p1, 'Write with arg does change path')
})

test_that('Component read errors correctly', {
  c = Component$new()
  expect_error(c$read('file://foo/bar'), 'Filesystem path does not exist.*')
});

test_that('Component writer creates the right directories', {
  c = Component$new()
  dir = tempdir()

  file = file.path(dir, 'bar', 'boop.txt')
  c$write(file)
  expect_true(dir.exists(file.path(dir, 'bar')), 'Parent directory is created')
  expect_false(file.exists(file), 'File is not written')

  dir2 = file.path(dir, 'bee')
  c$write(dir2)
  expect_equal(c$path, dir2, 'Path is set')
  expect_true(dir.exists(dir2), 'Directory is written')
});
