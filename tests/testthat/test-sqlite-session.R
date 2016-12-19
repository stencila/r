describe("SqliteSession", {
  s <- SqliteSession$new()

  it("can be constructed", {
    expect_equal(class(s)[1], "SqliteSession")
    expect_equal(s$type, "sqlite-session")
  })

  describe("execute()", {

    it('can have a data frame input which will be converted to a table and then removed', {
      r <- s$execute('.tables',list(
        data1=pack(data.frame(a=1:10)),
        data2=pack(data.frame(a=1:10))
      ))
      expect_equal(unpack(r$output), c('data1', 'data2'))

      r <- s$execute('.tables')
      expect_equal(length(unpack(r$output)), 0)
    })

    it('can "save" a tabular input using CREATE TABLE', {
      r <- s$execute('CREATE TABLE mydata AS SELECT * FROM data',list(
        data=pack(data.frame(a=1:10))
      ))
      r <- s$execute('.tables')
      expect_equal(unpack(r$output), c('mydata'))
    })

    it('can have input variables which can be used in SQL statements', {
      r <- s$execute('select avg(a) as "avg" from data where b=${which}',list(
        data=pack(data.frame(a=c(1,2,3), b=c('x','x','y'))),
        which=pack('x')
      ))
      expect_equal(unpack(r$output), data_frame(avg=1.5))
    })

  })
})
