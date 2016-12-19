describe('Component', {
  it("can be constructed", {
    c = Component$new()

    expect_equal(class(c)[1], "Component")
  })

  it('has an id', {
    c = Component$new()

    expect_equal(nchar(c$id), 64)
  })

  it('address is long on construction', {
    c = Component$new('/dir')
    expect_equal(c$address, 'file:///dir')
  })

  it('address defaults to name scheme', {
    c = Component$new()
    expect_equal(str_sub(c$address, 1, 7), 'name://')
  })

  it('address can be lengthend', {
    c = Component$new()

    expect_equal(c$long('new://document'), 'new://document')
    expect_equal(c$long('+document'), 'new://document')
    expect_equal(c$long('*aaaaaaaa'), 'name://aaaaaaaa')
    expect_equal(c$long('./report.docx'), paste0('file://', getwd(), '/report.docx'))
    expect_equal(c$long('/some/dir/report.docx'), 'file:///some/dir/report.docx')
    expect_equal(c$long('~/report.docx'), paste0('file://', path.expand('~'), '/report.docx'))
    expect_equal(c$long('https://foo.com/report.md'), 'https://foo.com/report.md')
    expect_equal(c$long('bb:foo/bar/report.md'), 'git://bitbucket.org/foo/bar/report.md')
    expect_equal(c$long('gh:/foo/bar/report.md'), 'git://github.com/foo/bar/report.md')
    expect_equal(c$long('gl://foo/bar/report.md'), 'git://gitlab.com/foo/bar/report.md')
    expect_equal(c$long('stats/t-test'), 'st://stats/t-test')
  })

  it('address can be shortened', {
    c = Component$new()

    expect_equal(c$short('new://document'), '+document')
    expect_equal(c$short('name://aaaaaaaa'), '*aaaaaaaa')
    expect_equal(c$short('file://report.docx'), 'file:report.docx')
    expect_equal(c$short('https://foo.com/report.md'), 'https:foo.com/report.md')
    expect_equal(c$short('git://bitbucket.org/foo/bar/report.md'), 'bb:foo/bar/report.md')
    expect_equal(c$short('git://github.com/foo/bar/report.md'), 'gh:foo/bar/report.md')
    expect_equal(c$short('git://gitlab.com/foo/bar/report.md'), 'gl:foo/bar/report.md')
    expect_equal(c$short('st://stats/t-test'), 'stats/t-test')
  })

  it('address can be lengthend and then shortened', {
    c = Component$new()
    ls = function(address) c$short(c$long(address))

    expect_equal(ls('+document'), '+document')
    expect_equal(ls('new://document'), '+document')
    expect_equal(ls('*aaaaaaaa'), '*aaaaaaaa')
    expect_equal(ls('name://aaaaaaaa'), '*aaaaaaaa')
    expect_equal(ls('gh:foo/bar/report.md'), 'gh:foo/bar/report.md')
    expect_equal(ls('gh:foo/bar/report.md@1.1.0'), 'gh:foo/bar/report.md@1.1.0')
  })

  it('address can be split', {
    c = Component$new()

    expect_equal(c$split('+document'), list(scheme='new', path='document', format=NA, version=as.character(NA)))
    expect_equal(c$split('*aaaaaaaa'), list(scheme='name', path='aaaaaaaa', format=NA, version=as.character(NA)))
    expect_equal(c$split('stats/t-test'), list(scheme='st', path='stats/t-test', format=NA, version=as.character(NA)))
    expect_equal(c$split('stats/t-test@1.1.0'), list(scheme='st', path='stats/t-test', format=NA, version='1.1.0'))
  })


  it('can be read and written', {
    p1 = paste0(tempfile(), '.tmp')
    p2 = paste0(tempfile(), '.tmp')
    # R doesn't actually creates the files, so do that
    cat('', file = p1)
    cat('', file = p2)

    c = Component$new()
    expect_equal(c$path, NULL, 'Initial path is NULL')

    c$read(p1)
    expect_equal(c$path, p1, 'Read with arg does sets path')

    c$read()
    expect_equal(c$path, p1, 'Read with no arg does not change path')

    c$write()
    expect_equal(c$path, p1, 'Write with no arg does not change path')

    c$write(p2)
    expect_equal(c$path, p2, 'Write with arg does change path')
  })

  it('read errors correctly', {
    c = Component$new()
    expect_error(c$read('file://foo/bar'), 'Filesystem path does not exist.*')
  })

  it('writer creates the right directories', {
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
  })
})
