import std.io;
import std.list;
import std.tuple;
import library.raylib;

struct Score {
    player1: int,
    player2: int
}

struct Game {
    score: Score,
    ball_position: (int, int),
    ball_velocity: (int, int),

    player1_position: int,
    player2_position: int
}

fn add_velocity(velocity: (int, int), addition: int) -> (int, int) {
    return (
        Math.clamp(if velocity.fst() > 0 {
            velocity.fst() + addition
        } else {
            velocity.fst() - addition
        }, -7, 7),
        Math.clamp(if velocity.snd() > 0 {
            velocity.snd() + addition
        } else {
            velocity.snd() - addition
        }, -7, 7)
    );
}

fn get_ai_movement(game: Game) -> int {
    let ball_y = game.ball_position.snd();
    let player_y = game.player1_position + 50; // center of the paddle

    // Predict the ball's future position based on its velocity
    let predicted_ball_y = ball_y + (game.ball_velocity.snd() * 20); // Predict 20 frames ahead

    let diff = predicted_ball_y - player_y;
    let movement = diff / 10; // Adjust sensitivity

    return Math.clamp(movement, -5, 5); // Allow faster movement for better tracking
}

fn display_score(score: int) -> Ray.Text.Text {
    let score_str = score.show();
    let text = Ray.Text.init()
        .content(score_str)
        .position(0, 0)
        .font_size(40)
        .color(0xFFFFFFFF);

    return text;
}

fn generate_next_velocity_component(base_velocity: int) -> int {
    return if Math.random(0, 10) > 5 { 
        base_velocity.negate()
    } else { 
        base_velocity
    };
}

fn generate_next_velocity(base_velocity: int) -> (int, int) {
    return (
        generate_next_velocity_component(base_velocity),
        generate_next_velocity_component(base_velocity)
    );
}

fn main() -> int {
    let screen_width = 1200;
    let screen_height = 800;

    Ray.init(screen_width, screen_height, "Pong Game");
    Ray.set_fps(60);
    
    let score = struct Score {
        player1: 0,
        player2: 0
    };

    let base_velocity = 3;

    let game = new struct Game {
        score: score,
        ball_position: (screen_width / 2, screen_height / 2),
        ball_velocity: (base_velocity, base_velocity),
        player1_position: (screen_height / 2) - 50,
        player2_position: (screen_height / 2) - 50
    }

    while Ray.is_running() {
        Ray.with_drawing(|| {
            Ray.clear(
                Ray.from_hex(0x62BFEDFF)
            );
            
            let middle_line = Ray.Rectangle.init()
                .position((screen_width / 2) - 5, 0)
                .size(10, screen_height)
                .color(Ray.from_hex(0xFFFFFF22));

            middle_line.draw();

            let score_text1 = display_score(game->score.player1);
            let score_text2 = display_score(game->score.player2);

            score_text1
                .position((screen_width / 4) - (score_text1.width() / 2), 20)
                .draw();

            score_text2
                .position((3 * screen_width / 4) - (score_text2.width() / 2), 20)
                .draw(); 
                

            let ball = Ray.Circle.init()
                .position(game->ball_position.fst(), game->ball_position.snd())
                .radius(10.0)
                .color(Ray.from_hex(0xFFFFFFFF));

            let player1 = Ray.Rectangle.init()
                .position(50, game->player1_position)
                .size(10, 100)
                .color(Ray.from_hex(0xFFFFFFFF));

            let player2 = Ray.Rectangle.init()
                .position(screen_width - 60, game->player2_position)
                .size(10, 100)
                .color(Ray.from_hex(0xFFFFFFFF));

            let ai_movement = get_ai_movement(*game);

            // Move player 1 (AI)
            game->player1_position = Math.clamp(
                game->player1_position + ai_movement,
                10,
                screen_height - 100 - 10
            );
            
            if Ray.Collision.circle_rectangle(
                ball,
                player1
            ) || Ray.Collision.circle_rectangle(
                ball,
                player2
            ) {
                game->ball_velocity = (
                    game->ball_velocity.fst() * 1.negate(),
                    game->ball_velocity.snd()
                );
                
                game->player1_position = Math.clamp(
                    game->player1_position + ai_movement,
                    10,
                    screen_height - 100 - 10
                );

                game->ball_velocity = add_velocity(game->ball_velocity, 1);
            }
            
            // Move player 2 with arrow keys codes
            if Ray.is_key_down(265) { // Up arrow
                game->player2_position = Math.clamp(
                    game->player2_position - 5,
                    10,
                    screen_height - 100 - 10
                );
            } else if Ray.is_key_down(264) { // Down arrow
                game->player2_position = Math.clamp(
                    game->player2_position + 5,
                    10,
                    screen_height - 100 - 10
                );
            }

            // Update ball position
            game->ball_position = game->ball_position.map(
                |x| x + game->ball_velocity.fst(),
                |y| y + game->ball_velocity.snd()
            );

            // Check for collisions with top and bottom walls
            if game->ball_position.snd() <= 10 || game->ball_position.snd() >= screen_height - 10 {
                game->ball_velocity = game->ball_velocity.map_second(
                    |y: int| y.negate()
                );
            }   

            let next_velocity = generate_next_velocity(base_velocity);

            // check for collisions with left and right walls (scoring)
            if game->ball_position.fst() <= 0 {
                game->score.player2 = game->score.player2 + 1;
                game->ball_position = (screen_width / 2, screen_height / 2);
                
                game->ball_velocity = next_velocity;
            } else if game->ball_position.fst() >= screen_width {   
                game->score.player1 = game->score.player1 + 1;
                game->ball_position = (screen_width / 2, screen_height / 2);
                
                game->ball_velocity = next_velocity;
            }

            player1.draw();
            player2.draw();
            ball.draw();
            
            unit
        });
    }

    Ray.close();

    return 0;
}
