`ifndef WHATEVER_PKG_SV
`define WHATEVER_PKG_SV

`cinclude "randoccm_filename.svh"
//<pl> my $string = 'hat'; # comment test
package whatever;

   logic www_w<$string> [<% ($random % 10) + 10 %> : <% $random % 10 %>];

   //<pl> if ($define_s) {
   function bit s(int x, int y);
      return (x < 0 && y > 0) ? 1 : 0;
   endfunction: s
   //<pl> }

   task fin();
      #100ns;
      $finish;
   endtask: fin

<perl>
use strict;
use warnings;

our $str1 = "//Some random string...\n";

my $a1 = "str1"; # a "symbolic reference" -- contains the name of the variable to reference

if ($a1 ne "") {
   no strict 'refs'; # since we have "use strict"
   print $$a1;
}
</perl>

endpackage: whatever

`endif // WHATEVER_PKG_SV
