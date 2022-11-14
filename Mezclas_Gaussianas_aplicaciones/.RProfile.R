setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::quartz.options(width = 6, height = 6))