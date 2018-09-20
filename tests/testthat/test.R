library(rhdf5client2)

context("HSDSSources")
test_that("Both servers found", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  doms <- domains(src.hsds, '/home/spollack')
  expect_true('/home/spollack/testzero.h5' %in% doms) 
  src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
  doms <- domains(src.chan)
  expect_true('neurons100k.h5s.channingremotedata.org' %in% doms) 
  doms <- domains(src.chan, 'newsubdir/test/hdfgroup/org')
  expect_true('newfile.newsubdir.test.h5s.channingremotedata.org' %in% doms) 
  
})

context("Files")
test_that("Files can be opened for reading", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
  f1 <- HSDSFile(src.hsds, '/home/spollack/testzero.h5')
  dsts <- listDatasets(f1)
  expect_true('/grpB/grpBA/dsetX' %in% dsts)
  f2 <- HSDSFile(src.chan, 'tenx_100k_sorted.h5s.channingremotedata.org')
  dsts <- listDatasets(f2)
  expect_true(dsts == c('/assay001'))
})

context("Datasets")
test_that("Data can be retrieved from Datasets", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
  f1 <- HSDSFile(src.hsds, '/home/spollack/testone.h5')
  f2 <- HSDSFile(src.chan, 'tenx_100k_sorted.h5s.channingremotedata.org')
  d1 <- HSDSDataset(f1, '/group0/dset1d')
  d2 <- HSDSDataset(f2, '/assay001')
  R <- c(4046,2087,4654,3193)

  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='JSON'), 1, sum)
  expect_true(all(R == A))
  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='binary'), 1, sum)
  expect_true(all(R == A))
  A <- apply(d2[1:4, 1:27998], 1, sum)
  expect_true(all(R == A))
  expect_true(sum(d1[1:20]) == 937)
})

context("DelayedArray subclass HSDSArray")
test_that("DelayedArray can be instantiated and accessed",  {
  R <- c(4046,2087,4654,3193)
  da <- HSDSArray('http://h5s.channingremotedata.org:5000', 'h5serv', 
        'tenx_full.h5s.channingremotedata.org', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
  da <- HSDSArray('http://hsdshdflab.hdfgroup.org', 'hsds', 
        '/shared/bioconductor/tenx_full.h5', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
})

context("Four-dimensional datasets")
test_that("Higher-dimensional dataset access works correctly",  {
  src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  rd <- HSDSDataset(HSDSFile(src, '/home/spollack/testone.h5'), '/group0/group1/group2/data4d')
  A <- getData(rd, list(3:4, 8:9, 5:6, 2:3))
  expect_true(sum(A) == 697)
})



