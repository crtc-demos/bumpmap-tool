(* Convert heightmap (intensity; whiter -> higher levels) into intensity-alpha
   (S, T) offsets.  *)

open Color

(* Return height of pixel, as float between 0 and 1.  *)

let get_height img xpos ypos =
  let pix = Rgba32.get img xpos ypos in
  (float_of_int (pix.color.Rgb.r + pix.color.Rgb.g + pix.color.Rgb.b))
  /. (3. *. 255.)

let x_slope img xpos ypos xsize =
  if xpos = 0 then
    let x0 = get_height img xpos ypos
    and x1 = get_height img (xpos + 1) ypos in
    x1 -. x0
  else if xpos = xsize - 1 then
    let x0 = get_height img (xpos - 1) ypos
    and x1 = get_height img xpos ypos in
    x1 -. x0
  else
    let x0 = get_height img (xpos - 1) ypos
    and x1 = get_height img xpos ypos
    and x2 = get_height img (xpos + 1) ypos in
    ((x2 -. x1) +. (x1 -. x0)) /. 2.0

let y_slope img xpos ypos ysize =
  if ypos = 0 then
    let y0 = get_height img xpos ypos
    and y1 = get_height img xpos (ypos + 1) in
    y1 -. y0
  else if ypos = ysize - 1 then
    let y0 = get_height img xpos (ypos - 1)
    and y1 = get_height img xpos ypos in
    y1 -. y0
  else
    let y0 = get_height img xpos (ypos - 1)
    and y1 = get_height img xpos ypos
    and y2 = get_height img xpos (ypos + 1) in
    ((y2 -. y1) +. (y1 -. y0)) /. 2.0

let slope_int s =
  let si = int_of_float (-.s *. 128.0) in
  let si = min si 127 in
  let si = max si (-128) in
  si + 128

let make_rgba32 img =
  match img with
    Images.Index8 i -> Index8.to_rgba32 i
  | Images.Index16 i -> Index16.to_rgba32 i
  | Images.Rgb24 i -> Rgb24.to_rgba32 i
  | Images.Rgba32 i -> i
  | Images.Cmyk32 i -> failwith "CMYK images unsupported"

let make_offset_img oimg img xsize ysize =
  for x = 0 to xsize - 1 do
    for y = 0 to ysize - 1 do
      let x_s = slope_int (x_slope img x y xsize)
      and y_s = slope_int (y_slope img x y ysize) in
      (* S-difference in the alpha channel, T-difference in the intensity
         channel.  *)
      Rgba32.set oimg x y { color = { Rgb.r = y_s; g = y_s; b = y_s };
			    alpha = x_s }
    done
  done

let string_of_img_format = function
    Images.Gif -> "gif"
  | Images.Bmp -> "bmp"
  | Images.Jpeg -> "jpeg"
  | Images.Tiff -> "tiff"
  | Images.Png -> "png"
  | Images.Xpm -> "xpm"
  | Images.Ppm -> "ppm"
  | Images.Ps -> "ps"

let _ =
  let infile = ref ""
  and outfile = ref "" in
  let argspec =
    ["-o", Arg.Set_string outfile, "Set output file"]
  and usage = "Usage: bumpmap infile -o outfile" in
  Arg.parse argspec (fun name -> infile := name) usage;
  if !infile = "" || !outfile = "" then begin
    Arg.usage argspec usage;
    exit 1
  end;
  let img = Images.load !infile [] in
  let xsize, ysize = Images.size img in
  Printf.printf "Got image: size %d x %d\n" xsize ysize;
  let offsetimg = Rgba32.create xsize ysize in
  make_offset_img offsetimg (make_rgba32 img) xsize ysize;
  let ofmt = Images.guess_format !outfile in
  Printf.printf "Saving as format: %s\n" (string_of_img_format ofmt);
  Images.save !outfile (Some ofmt) [] (Images.Rgba32 offsetimg)
