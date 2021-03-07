function sign, x
  on_error, 2
  return, 0 + (x gt 0) - (x lt 0)
end