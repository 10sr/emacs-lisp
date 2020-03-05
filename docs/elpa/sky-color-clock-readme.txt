Put sky-color-clock.el in a "load-path"ed directory, require this
script

  (require 'sky-color-clock)

and initialize sky-color-clock with your location's latitude

  (sky-color-clock-initialize 35) ; Tokyo, Japan

Then function `sky-color-clock' returns a propertized string,
which you may add to your `mode-line-format'.

  (push '(:eval (sky-color-clock)) (default-value 'mode-line-format))

You also may give sky-color-clock a Openweathermap API key

  (sky-color-clock-initialize-openweathermap-client "API-Key" 1850144) ; Tokyo's City ID

to enable weather icon (rain or snow), tmperature indicator and
reflect cloudiness to the sky color.
