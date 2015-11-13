% Notes on "Automatic Discovery of Linear Restraints Amond Variables of a Program"
% JMCT

Introduction
============

Consider a simple Bubble Sort:

```{.c}
void bubble_sort(int n, int* arr)
{
  int b = n;
  int j, t;
  while (b >= 1) {
    j = 1;
    t = 0;
    while (j <= (b - 1)) {
      if (arr[j] > arr[j+1]) {
        swap(j, j+1, arr); // no side-effects on anything but arr
        t = j;
      }
      j += 1;
    }

    if (t == 0)
      return;

  }
}
```

What are the relationships between the variables at different points in the program?
Using the analysis described by Cousot and Halbwachs we can actually determine some
restraints of the variables automatically. For example, during the inner `while` loop
we can statically determine the following relationships:

$$ b \leq n,\ t \geq 0,\ t+1 \leq j,\ j+1 \leq b $$

Information such as the above can help determine that array indices are within
bounds, or that there is the possibility of integer overflow, _at compile
time_.


Formal Representation of Linear Restraints Among Variables of a Program
=======================================================================


