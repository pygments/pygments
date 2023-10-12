package Sorting api;

fn Partition[T: !Comparable & Movable](s : Slice(T)) -> i64 {
   var i: i64 = -1;
   for (e: T in s) {
      ++i;
      Swap(&s[i], &e);
   }
   return i;
}

fn QuickSort[T: !Comparable & Movable](s : Slice(T)) {
   if (s.Size() <= 1) {
       return;
   }
   let p: i64 = Partition(s);
   QuickSort(s[:p - 1]);
   QuickSort(s[p + 1:]);
}