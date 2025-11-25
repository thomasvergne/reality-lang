#include <raylib.h>
#include <stdbool.h>
#include <stdint.h>

void init_window(int32_t width, int32_t height, char* title) {
  InitWindow(width, height, title);
}

void set_fps(int32_t fps) { SetTargetFPS(fps); }

bool should_close() { return WindowShouldClose(); }

void begin_drawing() { BeginDrawing(); }

void end_drawing() { EndDrawing(); }

void clear_background_color(int32_t r, int32_t g, int32_t b, int32_t a) {
  ClearBackground((Color){
      .r = (uint8_t)r, .g = (uint8_t)g, .b = (uint8_t)b, .a = (uint8_t)a});
}

void draw_rectangle(int32_t x, int32_t y, int32_t width, int32_t height,
                    int32_t r, int32_t g, int32_t b, int32_t a) {
  DrawRectangle(
      x, y, width, height,
      (Color){
          .r = (uint8_t)r, .g = (uint8_t)g, .b = (uint8_t)b, .a = (uint8_t)a});
}

void draw_text(char* text, int32_t x, int32_t y, int32_t fontSize, int32_t r,
               int32_t g, int32_t b, int32_t a) {
  DrawText(
      text, x, y, fontSize,
      (Color){
          .r = (uint8_t)r, .g = (uint8_t)g, .b = (uint8_t)b, .a = (uint8_t)a});
}

void draw_circle(int32_t centerX, int32_t centerY, float radius, int32_t r,
                 int32_t g, int32_t b, int32_t a) {
  DrawCircle(
      centerX, centerY, radius,
      (Color){
          .r = (uint8_t)r, .g = (uint8_t)g, .b = (uint8_t)b, .a = (uint8_t)a});
}

int32_t get_mouse_x() { return GetMouseX(); }

int32_t get_mouse_y() { return GetMouseY(); }

float get_mouse_wheel() { return GetMouseWheelMove(); }

bool check_collision_recs(int rect1_x, int rect1_y, int rect1_width,
                          int rect1_height, int rect2_x, int rect2_y,
                          int rect2_width, int rect2_height) {
  return CheckCollisionRecs(
      (Rectangle){.x = (float)rect1_x,
                  .y = (float)rect1_y,
                  .width = (float)rect1_width,
                  .height = (float)rect1_height},
      (Rectangle){.x = (float)rect2_x,
                  .y = (float)rect2_y,
                  .width = (float)rect2_width,
                  .height = (float)rect2_height});
}

bool check_collision_circles(float x1, float y1, float radius1, float x2,
                             float y2, float radius2) {
  return CheckCollisionCircles((Vector2){.x = x1, .y = y1}, radius1,
                               (Vector2){.x = x2, .y = y2}, radius2);
}

bool check_collision_circle_rec(float centerX, float centerY, float radius,
                                int rec_x, int rec_y, int rec_width,
                              int rec_height) {
  return CheckCollisionCircleRec((Vector2){.x = centerX, .y = centerY}, radius,
                                 (Rectangle){.x = (float)rec_x, .y = (float)rec_y, .width = (float)rec_width, .height = (float)rec_height});
}

int text_width(char* text, int32_t fontSize) {
  return MeasureText(text, fontSize);
}

int keyboard_input() {
    return GetKeyPressed();
}

bool is_key_down(int32_t keycode) { return IsKeyDown(keycode); }

void close_window() { CloseWindow(); }

int random_int(int32_t min, int32_t max) { return GetRandomValue(min, max); }

int32_t bshift_left(int32_t value, int32_t amount) { return value << amount; }

int32_t bshift_right(int32_t value, int32_t amount) { return value >> amount; }

int32_t and_bit(int32_t a, int32_t b) { return a & b; }

int32_t or_bit(int32_t a, int32_t b) { return a | b; }

int32_t not_bit(int32_t a) { return ~a; }
