val l = List(
  ("Guard 1", "blob", "trans", "file"),
  ("Guard 1", "blob2", "trans2", "file2"),
  ("Guard 1", "blob3", "trans3", "file3"),
  ("Guard 2", "blob4", "trans4", "file4"),
  ("Guard 2", "blob5", "trans5", "file5"),
)

l.map { case(s, b, t, f) => (s, (b,t,f) ) }


  //.groupBy { case(s, _) => s }