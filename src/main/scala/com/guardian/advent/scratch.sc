val s = new StringBuilder

val d = "abcde".toList.foldLeft(new StringBuilder){ case(sb, c) => sb.append(c)}
d.toString