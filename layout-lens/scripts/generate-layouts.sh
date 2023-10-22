shopt -s globstar  # Enable the globstar option to use **

for config in **/lens.json; do
  # Check if the path is a regular file
  if [ -f "$config" ]; then 
    # Replace the .json extension with a new extension .svg
    out="${config%.json}.svg"
    # Generate preview
    layout-lens "$config" "$out"
  fi
done
