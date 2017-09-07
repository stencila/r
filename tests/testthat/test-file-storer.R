describe('FileStorer', {

  it('has a constructor', {
    s <- FileStorer$new('test-dir-1')
    expect_equal(s$getDirectory(), file.path(getwd(), 'test-dir-1'))
    expect_equal(s$getMain(), NULL)
    expect_equal(s$getFiles(), I('main.md'))

    s <- FileStorer$new('test-dir-1/main.md')
    expect_equal(s$getDirectory(), file.path(getwd(), 'test-dir-1'))
    expect_equal(s$getMain(), 'main.md')
    expect_equal(s$getFiles(), I('main.md'))

    s <- FileStorer$new('~/some/dir')
    expect_equal(s$getDirectory(), path.expand('~/some/dir'))
    expect_equal(s$getMain(), NULL)

    s <- FileStorer$new('~/some/file.txt')
    expect_equal(s$getDirectory(), path.expand('~/some'))
    expect_equal(s$getMain(), 'file.txt')
  })

  it('has a readFile() method', {
    s <- FileStorer$new('test-dir-1')
    expect_equal(s$readFile('main.md'), 'Hello world!\n')

    s <- FileStorer$new('test-dir-1/main.md')
    expect_equal(s$readFile('main.md'), 'Hello world!\n')
  })

  it('has a writeFile() method', {
    s <- FileStorer$new('test-dir-1')
    s$writeFile('temp.txt', 'A tempororary file created by a test!\n')
    expect_equal(s$getFiles(), I(c('main.md', 'temp.txt')))
    expect_equal(s$readFile('temp.txt'), 'A tempororary file created by a test!\n')
    s$deleteFile('temp.txt')
  })

  it('has a getInfo() method', {
    s <- FileStorer$new('test-dir-1')
    expect_equal(s$getInfo(), list(
      dir = s$getDirectory(),
      main = s$getMain(),
      files = s$getFiles()
    ))
  })
})
