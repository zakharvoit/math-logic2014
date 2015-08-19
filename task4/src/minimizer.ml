open Verification

let minimize proof annotations =
  let n = Array.length proof in
  let visited = Array.make n false in
  visited.(n - 1) <- true;
  let result = ref [] in
  for i = n - 1 downto 0 do
    if visited.(i) then
      begin
	result := proof.(i) :: !result;
	match annotations.(i) with
	| ByModusPonens (a, b) -> (visited.(a) <- true; visited.(b) <- true)
	| ByRule1 a | ByRule2 a -> visited.(a) <- true
	| _ -> ()
      end
    else ()
  done;
  !result
