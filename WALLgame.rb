require "io/console"


# ================= CONFIG =================
WIDTH  = 80
HEIGHT = 24
FOV    = Math::PI / 3


MAX_DIST = 24.0
STEP     = 0.05


GRAVITY    = 0.08
JUMP_FORCE = 0.28


BLOCK_SIZE   = 10
STREET_WIDTH = 4


ACCEL    = 0.05
FRICTION = 0.88


WALLRUN_GRAVITY   = 0.025
WALLRUN_PUSH      = 0.065
WALLRUN_MIN_SPEED = 0.17
WALLRUN_MAX_TIME  = 30


MANTLE_LEDGE    = 0.35
MANTLE_MAX_RISE = 1.2
MANTLE_TIME     = 10


VAULT_MAX_HEIGHT = 0.6
VAULT_SPEED_MIN  = 0.18
VAULT_TIME       = 6


MAX_HEALTH        = 100.0
REGEN_RATE        = 0.3
REGEN_DELAY       = 50
SAFE_FALL         = 1.5
FALL_DAMAGE_SCALE = 28.0


FLOW_FOV_SCALE   = 0.0008
FLOW_FOV_MAX     = 0.25
FLOW_GAIN_MAX    = 2.0


# ================= PLAYER =================
def reset_player
  {
    px: 3.5, py: 3.5, pa: 0.0,
    pz: 0.0, vz: 0.0,
    vx: 0.0, vy: 0.0,
    on_ground: true,


    wallrunning: false,
    wall_side: nil,
    wall_time: 0,


    mantling: false,
    mantle_timer: 0,
    mantle_start_z: 0.0,
    mantle_end_z: 0.0,


    vaulting: false,
    vault_timer: 0,
    vault_start_z: 0.0,
    vault_end_z: 0.0,
    vault_cooldown: 0,


    pole_swing: false,
    pole_timer: 0,
    pole_x: 0.0,
    pole_y: 0.0,
    pole_angle_base: 0.0,


    hand_phase: 0.0,


    pitch: 0.0,
    tilt: 0.0,


    health: MAX_HEALTH,
    regen_timer: 0,
    air_peak: 0.0,
    was_airborne: false,


    flow: 0.0,
    flow_decay: 0.98,
    flowsense: true,


    on_ladder: false
  }
end


p = reset_player


# ================= UTILS =================
def clear
  print "\e[H\e[2J"
end


def noise(x, y)
  n = x * 73856093 ^ y * 19349663
  n = (n << 13) ^ n
  1.0 - ((n * (n * n * 15731 + 789221) + 1376312589) & 0x7fffffff) / 1073741824.0
end


def building_height(x, y)
  cell = BLOCK_SIZE + STREET_WIDTH
  bx = (x / cell).floor
  by = (y / cell).floor
  (((noise(bx, by) + 1) * 0.5) * 8).clamp(0, 8)
end


def in_building?(x, y)
  cell = BLOCK_SIZE + STREET_WIDTH
  x.floor % cell < BLOCK_SIZE && y.floor % cell < BLOCK_SIZE
end


def in_wall?(x, y)
  in_building?(x, y)
end


def ladder_at?(x, y)
  return false unless in_wall?(x, y)


  cell = BLOCK_SIZE + STREET_WIDTH
  lx = x.floor % cell
  ly = y.floor % cell


  edge = (
    lx == 0 || lx == BLOCK_SIZE - 1 ||
    ly == 0 || ly == BLOCK_SIZE - 1
  )


  edge && noise(x.floor, y.floor) > 0.6
end


def pole_at?(x, y)
  cell = BLOCK_SIZE + STREET_WIDTH
  gx = x.floor
  gy = y.floor
  lx = gx % cell
  ly = gy % cell


  corner_x = lx.between?(BLOCK_SIZE, BLOCK_SIZE + 1)
  corner_y = ly.between?(BLOCK_SIZE, BLOCK_SIZE + 1)
  in_street = !in_building?(x, y)


  in_street && corner_x && corner_y && noise(gx, gy) > 0.0
end


def ground_height(x, y)
  in_building?(x, y) ? building_height(x, y) : 0.0
end


PROP_NONE  = nil
PROP_AC    = :ac
PROP_VENT  = :vent
PROP_SKY   = :skylight
PROP_FENCE = :fence
PROP_DOG   = :dog


def rooftop_prop_at?(x, y)
  return PROP_NONE unless in_building?(x, y)


  cell = BLOCK_SIZE + STREET_WIDTH
  bx = (x.floor / cell).floor
  by = (y.floor / cell).floor


  n = noise(bx, by)


  case
  when n > 0.65 then PROP_AC
  when n > 0.50 then PROP_VENT
  when n > 0.35 then PROP_SKY
  when n > 0.20 then PROP_DOG
  when n > 0.05 then PROP_FENCE
  else PROP_NONE
  end
end


def prop_height(x, y)
  base = building_height(x, y)
  case rooftop_prop_at?(x, y)
  when PROP_AC    then base + 0.6
  when PROP_VENT  then base + 0.4
  when PROP_SKY   then base + 0.2
  when PROP_FENCE then base + 0.8
  when PROP_DOG   then base + 0.5
  else nil
  end
end


def prop_char(prop)
  case prop
  when PROP_AC    then "H"
  when PROP_VENT  then "O"
  when PROP_SKY   then "□"
  when PROP_FENCE then "#"
  when PROP_DOG   then "&"
  else "?"
  end
end


def prop_blocking?(x, y)
  rooftop_prop_at?(x, y) != PROP_NONE
end


def key
  STDIN.echo = false
  STDIN.raw!
  STDIN.read_nonblock(3, exception: false)
ensure
  STDIN.echo = true
  STDIN.cooked!
end


# FLOWSENSE DISABLED
def flowsense_char(base, p, rx, ry)
  base
end


def movement_locked?(p)
  p[:vaulting] || p[:mantling] || p[:pole_swing]
end


# ================= TITLE =================
def title_screen
  clear
  puts "\n      WALL-(game of jumps)\n\n      -lets run!\n\n"
  puts "   W A S D  move"
  puts "   ← →      turn"
  puts "   ↑ ↓      look up/down"
  puts "   SPACE    jump / wall / pole"
  puts "   X        quit\n\n"
  puts "   Press any key..."
  STDIN.getc
end


title_screen
state = :playing


puts "Initializing WALL..."
sleep 0.1
clear
# ================= GAME LOOP =================
loop do
  clear


  case state
  when :playing
    # ---------- INPUT ----------
    k = key
    case k
    when "x","X" then state = :quit
    when "\e[D"  then p[:pa] -= 0.1
    when "\e[C"  then p[:pa] += 0.1


    when "\e[A"  then p[:pitch] = (p[:pitch] - 0.1).clamp(-1.0, 1.0)
    when "\e[B"  then p[:pitch] = (p[:pitch] + 0.1).clamp(-1.0, 1.0)


    when "w"
      if p[:on_ladder]
        p[:pz] += 0.12
      else
        p[:vx] += Math.cos(p[:pa]) * ACCEL unless movement_locked?(p)
        p[:vy] += Math.sin(p[:pa]) * ACCEL unless movement_locked?(p)
      end


    when "s"
      if p[:on_ladder]
        p[:pz] -= 0.12
      end


    when "a"
      p[:vx] += Math.cos(p[:pa] - Math::PI/2) * ACCEL unless movement_locked?(p)
      p[:vy] += Math.sin(p[:pa] - Math::PI/2) * ACCEL unless movement_locked?(p)


    when "d"
      p[:vx] += Math.cos(p[:pa] + Math::PI/2) * ACCEL unless movement_locked?(p)
      p[:vy] += Math.sin(p[:pa] + Math::PI/2) * ACCEL unless movement_locked?(p)


    when " "
      if p[:pole_swing]
        p[:pole_swing] = false
        swing_phase = p[:pole_timer] / 12.0
        angle = p[:pole_angle_base] + Math.sin(swing_phase * Math::PI) * 0.6
        launch_angle = angle + Math::PI / 2
        p[:vx] = Math.cos(launch_angle) * 0.55
        p[:vy] = Math.sin(launch_angle) * 0.55
        p[:vz] = 0.25


      elsif p[:wallrunning]
        s = p[:wall_side] == :left ? 1 : -1
        p[:vx] += Math.cos(p[:pa] + s * Math::PI/2) * WALLRUN_PUSH
        p[:vy] += Math.sin(p[:pa] + s * Math::PI/2) * WALLRUN_PUSH
        p[:vz] = JUMP_FORCE * 0.9
        p[:wallrunning] = false


      elsif p[:on_ground] && !movement_locked?(p)
        p[:vz] = JUMP_FORCE
        p[:on_ground] = false
      end
    end
speed = Math.hypot(p[:vx], p[:vy])


    # ---------- FLOW ----------
    gain = 0.0
    gain += speed * 0.04 if speed > 0.05
    gain += 0.3 if p[:wallrunning]
    gain += 0.2 if !p[:on_ground] && p[:vz] > 0.05
    gain += 0.4 if p[:vz] < -0.25
    gain += 0.5 if p[:was_airborne] && p[:on_ground]
    gain = gain.clamp(0.0, FLOW_GAIN_MAX)
    p[:flow] = (p[:flow] * p[:flow_decay]) + gain
    p[:flow] = p[:flow].clamp(0, 999)


    # ---------- WALLRUN DETECT ----------
    if !p[:on_ground] && !p[:wallrunning] && !movement_locked?(p) && speed > WALLRUN_MIN_SPEED
      lx = p[:px] + Math.cos(p[:pa] - Math::PI/2) * 0.3
      rx = p[:px] + Math.cos(p[:pa] + Math::PI/2) * 0.3
      if in_wall?(lx, p[:py])
        p[:wallrunning] = true
        p[:wall_side] = :left
        p[:wall_time] = 0
        p[:vz] = 0
      elsif in_wall?(rx, p[:py])
        p[:wallrunning] = true
        p[:wall_side] = :right
        p[:wall_time] = 0
        p[:vz] = 0
      end
    end


    # ---------- WALLRUN CAMERA TILT ----------
    if p[:wallrunning]
      target_tilt = (p[:wall_side] == :left ? -0.55 : 0.55)
    else
      target_tilt = 0.0
    end
    p[:tilt] = p[:tilt] * 0.80 + target_tilt * 0.20


    # ---------- POLE GRAB ----------
    if !p[:on_ground] && !p[:pole_swing] && !p[:vaulting] && !p[:mantling]
      fx = p[:px] + Math.cos(p[:pa]) * 0.4
      fy = p[:py] + Math.sin(p[:pa]) * 0.4
      if pole_at?(fx, fy)
        p[:pole_swing] = true
        p[:pole_timer] = 0
        p[:pole_x], p[:pole_y] = fx, fy
        p[:pole_angle_base] = Math.atan2(p[:py] - fy, p[:px] - fx)
        p[:vx] = p[:vy] = 0
        p[:vz] = 0
      end
    end


    # ---------- AUTO VAULT ----------
    if !p[:vaulting] && !p[:mantling] && p[:on_ground] &&
       speed > VAULT_SPEED_MIN && p[:vault_cooldown] <= 0


      fx = p[:px] + Math.cos(p[:pa]) * 0.5
      fy = p[:py] + Math.sin(p[:pa]) * 0.5
      dh = ground_height(fx, fy) - ground_height(p[:px], p[:py])


      if dh > 0.05 && dh <= VAULT_MAX_HEIGHT
        p[:vaulting] = true
        p[:mantling] = false
        p[:pole_swing] = false
        p[:vault_timer] = 0
        p[:vault_start_z] = p[:pz]
        p[:vault_end_z] = ground_height(fx, fy)
        p[:flow] += 8
      end
    end


    # ---------- MANTLE DETECT ----------
    if !p[:mantling] && !p[:vaulting] && !p[:on_ground] && p[:vz] <= 0 && !p[:pole_swing]
      fx = p[:px] + Math.cos(p[:pa]) * 0.5
      fy = p[:py] + Math.sin(p[:pa]) * 0.5
      base  = ground_height(p[:px], p[:py])
      ledge = ground_height(fx, fy)
      rise  = ledge - base
      if rise >= MANTLE_LEDGE && rise <= MANTLE_MAX_RISE
        p[:mantling] = true
        p[:vaulting] = false
        p[:pole_swing] = false
        p[:mantle_timer] = 0
        p[:mantle_start_z] = p[:pz]
        p[:mantle_end_z] = ledge + 0.05
        p[:vx] = p[:vy] = 0
        p[:vz] = 0
      end
    end
# ---------- VAULT MOTION ----------
    if p[:vaulting]
      t = p[:vault_timer] / VAULT_TIME.to_f
      arc = Math.sin(t * Math::PI) * 0.25
      p[:pz] = p[:vault_start_z] * (1 - t) + p[:vault_end_z] * t + arc


      nx = p[:px] + Math.cos(p[:pa]) * 0.12
      ny = p[:py] + Math.sin(p[:pa]) * 0.12
      p[:px] = nx unless in_wall?(nx, p[:py])
      p[:py] = ny unless in_wall?(p[:px], ny)


      p[:vault_timer] += 1
      if p[:vault_timer] >= VAULT_TIME
        p[:vaulting] = false
        p[:vault_cooldown] = 12
        p[:on_ground] = true
      end
    end
    p[:vault_cooldown] -= 1 if p[:vault_cooldown] > 0


    # ---------- MANTLE MOTION ----------
    if p[:mantling]
      t = p[:mantle_timer] / MANTLE_TIME.to_f
      arc = Math.sin(t * Math::PI) * 0.18
      p[:pz] = p[:mantle_start_z] * (1 - t) + p[:mantle_end_z] * t + arc


      nx = p[:px] + Math.cos(p[:pa]) * 0.06
      ny = p[:py] + Math.sin(p[:pa]) * 0.06
      p[:px] = nx unless in_wall?(nx, p[:py])
      p[:py] = ny unless in_wall?(p[:px], ny)


      p[:mantle_timer] += 1
      if p[:mantle_timer] >= MANTLE_TIME
        p[:mantling] = false
        p[:pz] = p[:mantle_end_z]
        p[:vz] = 0
        p[:on_ground] = true
      end
    end


    # ---------- POLE SWING ----------
    if p[:pole_swing]
      p[:pole_timer] += 1
      swing_phase = p[:pole_timer] / 12.0
      angle = p[:pole_angle_base] + Math.sin(swing_phase * Math::PI) * 0.6
      r = 0.45
      p[:px] = p[:pole_x] + Math.cos(angle) * r
      p[:py] = p[:pole_y] + Math.sin(angle) * r
      p[:hand_phase] += 0.4
      if p[:pole_timer] >= 12
        p[:pole_swing] = false
        launch_angle = angle + Math::PI / 2
        p[:vx] = Math.cos(launch_angle) * 0.45
        p[:vy] = Math.sin(launch_angle) * 0.45
        p[:vz] = 0.22
      end
    end


    # ---------- PHYSICS ----------
    unless movement_locked?(p)
      if p[:on_ladder]
        p[:vz] = 0
      elsif p[:wallrunning]
        p[:vz] -= WALLRUN_GRAVITY
        p[:wall_time] += 1
        if p[:wall_time] > WALLRUN_MAX_TIME
          p[:wallrunning] = false
          p[:vz] -= GRAVITY
        end
      else
        p[:vz] -= GRAVITY
      end
      p[:pz] += p[:vz]
    end


    # ---------- GROUND COLLISION + FALL DAMAGE ----------
    roof = ground_height(p[:px], p[:py])
    if p[:pz] <= roof
      if p[:was_airborne]
        fall = p[:air_peak] - roof
        if fall > SAFE_FALL
          p[:health] -= (fall - SAFE_FALL) * FALL_DAMAGE_SCALE
          p[:health] = [p[:health], 0.0].max
          p[:regen_timer] = REGEN_DELAY
        end
      end
      p[:pz] = roof
      p[:vz] = 0
      p[:on_ground] = true
      p[:wallrunning] = false
      p[:wall_time] = 0
      p[:air_peak] = 0
      p[:was_airborne] = false
    else
      p[:on_ground] = false
      p[:air_peak] = [p[:air_peak], p[:pz]].max unless movement_locked?(p)
      p[:was_airborne] = true
    end


    # ---------- LADDER DETECTION ----------
    p[:on_ladder] = ladder_at?(p[:px], p[:py])
# ---------- MOVE (with "unstick" logic) ----------
    unless movement_locked?(p)
      p[:vx] *= FRICTION
      p[:vy] *= FRICTION


      nx = p[:px] + p[:vx]
      ny = p[:py] + p[:vy]


      cur_blocked_x = in_wall?(p[:px], p[:py]) || pole_at?(p[:px], p[:py]) || prop_blocking?(p[:px], p[:py])
      new_blocked_x = in_wall?(nx, p[:py])    || pole_at?(nx, p[:py])    || prop_blocking?(nx, p[:py])


      cur_blocked_y = in_wall?(p[:px], p[:py]) || pole_at?(p[:px], p[:py]) || prop_blocking?(p[:px], p[:py])
      new_blocked_y = in_wall?(p[:px], ny)    || pole_at?(p[:px], ny)    || prop_blocking?(p[:px], ny)


      p[:px] = nx unless new_blocked_x && !cur_blocked_x
      p[:py] = ny unless new_blocked_y && !cur_blocked_y
    end


    # ---------- HEALTH ----------
    if p[:regen_timer] > 0
      p[:regen_timer] -= 1
    else
      p[:health] = [p[:health] + REGEN_RATE, MAX_HEALTH].min
      p[:health] = [p[:health], 0.0].max
    end
    state = :dead if p[:health] <= 0


    # ---------- INTERACTION STATE FOR HANDS ----------
    interaction_mode  = nil
    interaction_hit_x = nil
    interaction_hit_y = nil


    if p[:mantling]
      interaction_mode = :mantle
    elsif p[:vaulting]
      interaction_mode = :vault
    elsif p[:pole_swing]
      dx = p[:pole_x] - p[:px]
      dy = p[:pole_y] - p[:py]
      pole_angle = Math.atan2(dy, dx)
      diff = Math.atan2(Math.sin(pole_angle - p[:pa]), Math.cos(pole_angle - p[:pa]))
      interaction_mode = diff < 0 ? :pole_left : :pole_right
    end
# ---------- RENDER ----------
    buf = Array.new(HEIGHT) { " " * WIDTH }


    cam = (HEIGHT / 2) +
          (p[:pitch] * (HEIGHT / 3.0)) +
          (p[:tilt]  * 5)


    fov_now = FOV + (p[:flow] * FLOW_FOV_SCALE).clamp(0, FLOW_FOV_MAX)


    WIDTH.times do |x|
      ra = p[:pa] - fov_now/2 + x.to_f/WIDTH * fov_now
      dist = 0.0
      rx, ry = p[:px], p[:py]
      hit = false
      hit_pole = false
      hit_prop = false
      hit_prop_type = nil
      hit_ladder = false


      while dist < MAX_DIST
        rx += Math.cos(ra) * STEP
        ry += Math.sin(ra) * STEP
        dist += STEP


        if ladder_at?(rx, ry)
          hit = true
          hit_ladder = true
          break
        end


        if in_wall?(rx, ry)
          hit = true
          break
        end


        prop = rooftop_prop_at?(rx, ry)
        if prop != PROP_NONE
          hit = true
          hit_prop = true
          hit_prop_type = prop
          break
        end


        if pole_at?(rx, ry)
          hit = true
          hit_pole = true
          break
        end
      end


      h = hit ? (HEIGHT / (dist + 0.1)).to_i : 0


      if interaction_hit_x.nil? && hit
        if interaction_mode == :mantle || interaction_mode == :vault
          unless hit_pole
            interaction_hit_x = x
            interaction_hit_y = cam - h/2
          end
        elsif interaction_mode == :pole_left || interaction_mode == :pole_right
          if hit_pole
            interaction_hit_x = x
            interaction_hit_y = cam - h/2
          end
        end
      end


      HEIGHT.times do |y|
        buf[y][x] =
          if !hit
            y < cam ? "`" : "."
          elsif y < cam - h/2
            "`"
          elsif y > cam + h/2
            "."
          else
            shade =
              if hit_ladder
                "T"
              elsif hit_pole
                "|"
              elsif hit_prop
                prop_char(hit_prop_type)
              elsif dist < 3
                "█"
              elsif dist < 6
                "▓"
              elsif dist < 10
                "▒"
              else
                "░"
              end
            flowsense_char(shade, p, rx, ry)
          end
      end
    end
# ---------- HANDS (object-attached or HUD) ----------
    if interaction_hit_x
      ((interaction_hit_x - 1)..(interaction_hit_x + 1)).each do |hx|
        next unless hx.between?(0, WIDTH - 1)


        case interaction_mode
        when :mantle, :vault
          hy = interaction_hit_y
          buf[hy][hx] = '@' if hy && hy.between?(0, HEIGHT - 1)


        when :pole_left
          hy = interaction_hit_y + 1
          buf[hy][hx] = '@' if hy && hy.between?(0, HEIGHT - 1) && hx <= interaction_hit_x


        when :pole_right
          hy = interaction_hit_y + 1
          buf[hy][hx] = '@' if hy && hy.between?(0, HEIGHT - 1) && hx >= interaction_hit_x
        end
      end


    else
      # ---------- HUD HANDS ----------
      p[:hand_phase] += speed * 0.6 + 0.03
      p[:hand_phase] %= (Math::PI * 2)


      run_bob = Math.sin(p[:hand_phase]) * (1 + speed * 6)


      lateral = -Math.sin(p[:pa]) * p[:vx] + Math.cos(p[:pa]) * p[:vy]
      strafe  = lateral * 20.0


      sway = Math.sin(p[:hand_phase] * 0.8) * (2 + speed * 4) + strafe


      air_offset =
        if !p[:on_ground] && !movement_locked?(p)
          -2 + p[:vz] * 10
        else
          0
        end


      special_x = 0.0
      special_y = 0.0


      if p[:vaulting]
        t = p[:vault_timer] / VAULT_TIME.to_f
        special_y += -6 + Math.sin(t * Math::PI) * 4
        special_x += Math.sin(t * Math::PI) * 3
      end


      if p[:mantling]
        t = p[:mantle_timer] / MANTLE_TIME.to_f
        special_y += -8 + Math.sin(t * Math::PI) * 6
        special_x += Math.sin(t * Math::PI * 0.5) * 2
      end


      if p[:pole_swing]
        swing_phase = p[:pole_timer] / 12.0
        arc = Math.sin(swing_phase * Math::PI)


        dx = p[:pole_x] - p[:px]
        dy = p[:pole_y] - p[:py]
        pole_angle = Math.atan2(dy, dx)


        angle_diff = pole_angle - p[:pa]
        angle_diff = Math.atan2(Math.sin(angle_diff), Math.cos(angle_diff))


        special_x += Math.sin(angle_diff) * 10
        special_x += arc * 8
        special_y += -6 + arc * 4
      end


      if p[:wallrunning]
        special_x += (p[:wall_side] == :left ? -3 : 3)
        special_y -= 2
      end


      hy = (HEIGHT - 4 + run_bob + air_offset + special_y + p[:pitch] * 4).to_i
      hy = hy.clamp(HEIGHT - 10, HEIGHT - 2)


      cx = (WIDTH / 2 + sway + special_x).to_i
      cx = cx.clamp(10, WIDTH - 10)


      left_hand  = "////>"
      right_hand = "<\\\\\\\\"


      lh_x = cx - 6
      rh_x = cx + 2


      [[left_hand, lh_x], [right_hand, rh_x]].each do |hand, base_x|
        hand.chars.each_with_index do |c, j|
          xh = base_x + j
          yh = hy
          next unless yh.between?(0, HEIGHT - 1) && xh.between?(0, WIDTH - 1)
          buf[yh][xh] = c
        end
      end
    end


    # ---------- HUD ----------
    "HP: #{p[:health].to_i}".chars.each_with_index { |c, i| buf[0][i] = c }
    "FLOW: #{p[:flow].to_i}".chars.each_with_index { |c, i| buf[1][i] = c }
if p[:on_ladder]
  "CLIMB HERE".chars.each_with_index { |c, i| buf[2][i] = c }
end


    puts buf.join("\n")
    sleep 0.016
when :dead
    clear
    puts "\nYou stopped jumping.\n\n[R] respawn\n[X] quit"
    case key
    when "r","R" then p = reset_player; state = :playing
    when "x","X" then state = :quit
    end


  when :quit
    clear
    puts "You stopped jumping."
    break
  end
end
