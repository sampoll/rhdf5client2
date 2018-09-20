library(rhdf5client2)

context("Sources")
test_that("Both servers found", {
  src.hsds <- Source('http://hsdshdflab.hdfgroup.org')
  doms <- domains(src.hsds, '/home/spollack')
  expect_true('/home/spollack/testzero.h5' %in% doms) 
  src.chan <- Source('http://h5s.channingremotedata.org:5000', 'h5serv')
  doms <- domains(src.chan)
  expect_true('neurons100k.h5s.channingremotedata.org' %in% doms) 
  doms <- domains(src.chan, 'newsubdir/test/hdfgroup/org')
  expect_true('newfile.newsubdir.test.h5s.channingremotedata.org' %in% doms) 
  
})

context("Files")
test_that("Files can be opened for reading", {
  src.hsds <- Source('http://hsdshdflab.hdfgroup.org')
  src.chan <- Source('http://h5s.channingremotedata.org:5000', 'h5serv')
  f1 <- File(src.hsds, '/home/spollack/testzero.h5')
  dsts <- listDatasets(f1)
  expect_true('/grpB/grpBA/dsetX' %in% dsts)
  f2 <- File(src.chan, 'tenx_100k_sorted.h5s.channingremotedata.org')
  dsts <- listDatasets(f2)
  expect_true(dsts == c('/assay001'))
})

context("Datasets")
test_that("Data can be retrieved from Datasets", {
  src.hsds <- Source('http://hsdshdflab.hdfgroup.org')
  src.chan <- Source('http://h5s.channingremotedata.org:5000', 'h5serv')
  f1 <- File(src.hsds, '/home/spollack/testone.h5')
  f2 <- File(src.chan, 'tenx_100k_sorted.h5s.channingremotedata.org')
  d1 <- Dataset(f1, '/group0/dset1d')
  d2 <- Dataset(f2, '/assay001')
  R <- c(4046,2087,4654,3193)

  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='JSON'), 1, sum)
  expect_true(all(R == A))
  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='binary'), 1, sum)
  expect_true(all(R == A))
  A <- apply(d2[1:4, 1:27998], 1, sum)
  expect_true(all(R == A))
  expect_true(sum(d1[1:20]) == 937)
})

context("DelayedArray subclass RHDF5Array")
test_that("DelayedArray can be instantiated and accessed",  {
  R <- c(4046,2087,4654,3193)
  da <- RHDF5Array('http://h5s.channingremotedata.org:5000', 'h5serv', 
        'tenx_full.h5s.channingremotedata.org', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
  da <- RHDF5Array('http://hsdshdflab.hdfgroup.org', 'hsds', 
        '/shared/bioconductor/tenx_full.h5', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
})

context("Four-dimensional datasets")
test_that("Higher-dimensional dataset access works correctly",  {
  src <- Source('http://hsdshdflab.hdfgroup.org')
  rd <- Dataset(File(src, '/home/spollack/testone.h5'), '/group0/group1/group2/data4d')
  A <- getData(rd, list(3:4, 8:9, 5:6, 2:3))
  expect_true(sum(A) == 697)
  dt <- Dataset(File(src, '/home/spollack/testone.h5'), '/group0/group1/dataR')
  B <- getData(dt, list(c(4), c(2, 3, 5, 6), c(5), 1:3))
  R <- array(c(3140, 3240, 3440, 3540, 3141, 3241, 3441, 3541, 3142, 
      3242, 3442, 3542), dim=c(4,3))
  expect_true(all(B == R))

})

context("Decomposition into slices")
test_that("Bad slices rejected",  {
  tf <- checkSlices(c(10, 20, 30), c('5:', ':', ':8'))
  ok <- c('4:10:1', '0:20:1', '0:8:1')
  expect_true(all(unlist(tf) == ok))
  expect_error(checkSlices(c(10, 20, 30), c('5:20', ':', ':8'),
    regexp='stop out of range'))
  expect_error(checkSlices(c(10, 20, 30), c('10:5', ':', ':8'),
    regexp='slice stop less than slice start'))
  expect_error(checkSlices(c(10, 20, 30), c('5:10,0.5', ':', ':8'),
    regexp='malformed slice'))
})





