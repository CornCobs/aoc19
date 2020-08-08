$max_signal = 0

foreach ($perm in python day7.py) {
  echo "perm: $perm"
  $signal = 0
  foreach ($setting in $perm.Split(",")) {
    $signal = $setting, $signal | stack exec aoc 
  }
  if ([int]$signal -gt $max_signal) {
    $max_signal = [int]$signal
  }
  echo "signal is $signal, max signal is now $max_signal"
}

$max_signal