module Day23 where 

import ST.IntCodeST

network opcodes = 
  let outputs = intcode opcodes <$> zipWith (:) [0..49] inputs 
      inputs  = 
