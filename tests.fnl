(local {: run} (require :faith))

(run [
      :chip8.machine-tests
      :chip8.memory-tests
      ]);{:done #(print $...) :exit #(print $...)})


