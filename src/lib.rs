//! A simple plugin and components for 2d/3d flying cameras in Bevy.
//!
//! # 3D
//!
//! Movement system is based on Minecraft, flying along the horizontal plane no matter the mouse's vertical angle, with two extra buttons for moving vertically.
//!
//! Default keybinds are:
//! - <kbd>W</kbd> / <kbd>A</kbd> / <kbd>S</kbd> / <kbd>D</kbd> - Move along the horizontal plane
//! - Shift - Move downward
//! - Space - Move upward
//!
//! ## Example
//! ```no_compile
//! use bevy::prelude::*;
//! use bevy_fly_camera::{FlyCamera, FlyCameraPlugin};
//!
//! fn setup(commands: &mut Commands) {
//!	  commands
//!     .spawn(Camera3dBundle::default())
//!     .with(FlyCamera::default());
//! }
//!
//! fn main() {
//!	  App::build()
//!     .add_plugins(DefaultPlugins)
//!     .add_startup_system(setup.system())
//!     .add_plugin(FlyCameraPlugin)
//!     .run();
//! }
//! ```
//!
//! There's also a basic piece of example code included in `/examples/basic.rs`
//!
//! # 2D
//! Movement system only uses the keyboard to move in all four directions across the 2d plane.
//!
//! The default keybinds are:
//! - <kbd>W</kbd> / <kbd>A</kbd> / <kbd>S</kbd> / <kbd>D</kbd> - Move along the 2d plane
//!
//! ## Example
//! ```no_compile
//! use bevy::prelude::*;
//! use bevy_fly_camera::{FlyCamera2d, FlyCameraPlugin};
//! ```
//! ```no_compile
//!	commands
//!   .spawn(Camera2dBundle::default())
//!   .with(FlyCamera2d::default());
//! ```
//!
//! There's also a basic piece of example code included in `/examples/2d.rs`

use bevy::{ 
	input::{
		mouse::{ MouseMotion, MouseScrollUnit, MouseWheel },
		prelude::*,
	},
	prelude::*,
	render::camera::{*, self}
};
use bevy_mod_picking::*;
use lerp::Lerp;
use cam2d::camera_2d_movement_system;
use util::movement_axis;

mod cam2d;
mod util;

pub use cam2d::FlyCamera2d;

#[derive(Component)]
pub struct CenterPick;

#[derive(Component)]
pub struct CenterPickRaycast;

#[derive(Component)]
pub struct CenterPickRaycastParent;

#[derive(Component, Default)]
pub struct ReaderData {
	pub glyph_width: f32,
	pub glyph_height: f32,
	pub rows: u32,
	pub columns: u32,
}

#[derive(Component, Default)]
pub struct Row(pub u32);

#[derive(Component, Default)]
pub struct Column(pub u32);


type PickingObject = bevy_mod_picking::RayCastSource<PickingRaycastSet>;

/// A set of options for initializing a FlyCamera.
/// Attach this component to a [`Camera3dBundle`](https://docs.rs/bevy/0.4.0/bevy/prelude/struct.Camera3dBundle.html) bundle to control it with your mouse and keyboard.
/// # Example
/// ```no_compile
/// fn setup(mut commands: Commands) {
///	  commands
///     .spawn(Camera3dBundle::default())
///     .with(FlyCamera::default());
/// }

#[derive(Component)]
pub struct FlyCamera {
	/// The speed the FlyCamera accelerates at. Defaults to `1.0`
	pub accel: f32,
	/// The maximum speed the FlyCamera can move at. Defaults to `0.5`
	pub max_speed: f32,
	/// The sensitivity of the FlyCamera's motion based on mouse movement. Defaults to `3.0`
	pub sensitivity: f32,
	/// The amount of deceleration to apply to the camera's motion. Defaults to `1.0`
	pub friction: f32,
	///
	pub zoom_sensitivity: f32,
	///
	pub vertical_scroll_easing_seconds: f32,
	///
	pub horizontal_scroll_easing_seconds: f32,
	///
	pub translation_easing_seconds: f32,
	///
	pub rotation_easing_seconds: f32,
	///
	pub zoom_easing_seconds: f32,
	///
	pub lean_easing_seconds: f32,
	///
	pub lean_reset_easing_seconds: f32,
	/// The current pitch of the FlyCamera in degrees. This value is always up-to-date, enforced by [FlyCameraPlugin](struct.FlyCameraPlugin.html)
	pub pitch: f32,
	/// The current pitch of the FlyCamera in degrees. This value is always up-to-date, enforced by [FlyCameraPlugin](struct.FlyCameraPlugin.html)
	pub yaw: f32,
	///
	pub zoom: f32,
	///
	pub vertical_scroll: f32,
	///
	pub horizontal_scroll: f32,
	///
	pub column: u32,
	///
	pub row: u32,
	///
	pub column_scroll_accum: f32,
	///
	pub row_scroll_accum: f32,
	///
	pub column_scroll_mouse_quantized: bool,
	///
	pub row_scroll_mouse_quantized: bool,
	///
	pub slowly_quantize_camera_position: bool,
	///
	pub slow_quatizing_easing_seconds: f32,
	///
	pub target_translation: Vec3,
	///
	pub target_rotation: Quat,
	///
	pub key_scroll_delay_seconds: f32,
	///
	pub key_scroll_delay_column_inc: f32,
	///
	pub key_scroll_delay_column_dec: f32,
	///
	pub key_scroll_delay_row_inc: f32,
	///
	pub key_scroll_delay_row_dec: f32,
	/// The current velocity of the FlyCamera. This value is always up-to-date, enforced by [FlyCameraPlugin](struct.FlyCameraPlugin.html)
	pub velocity: Vec3,
	/// Key used to move forward. Defaults to <kbd>W</kbd>
	pub key_forward: KeyCode,
	/// Key used to move backward. Defaults to <kbd>S</kbd>
	pub key_backward: KeyCode,
	/// Key used to move left. Defaults to <kbd>A</kbd>
	pub key_left: KeyCode,
	/// Key used to move right. Defaults to <kbd>D</kbd>
	pub key_right: KeyCode,
	/// Key used to move up. Defaults to <kbd>Space</kbd>
	pub key_up: KeyCode,
	/// Key used to move forward. Defaults to <kbd>LShift</kbd>
	pub key_down: KeyCode,
	/// Key used to toggle perspective mode on camera. Defaults to <kbd>Return</kbd>
	pub key_perspective: KeyCode,
	/// Key used as modifier along with key_perspective. Optional. Defaults to Some(<kbd>LControl</kbd>)
	pub mod_perspective: Option<KeyCode>,
	/// If `true` camera rotation gets reset to 0 when projection switches to orthographic from perspective. Defaults to `true`
	pub reset_rotation_on_ortho: bool,
	/// indicates if camera has perspective projection. Switches to orthographic when `false`
	pub perspective: bool, 
	/// If `false`, disable keyboard control of the camera. Defaults to `true`
	pub enabled_translation: bool,
	/// If `false`, disable mouse control of the camera. Defaults to `true`
	pub enabled_rotation: bool,
	///
	pub enabled_zoom: bool,
	///
	pub enabled_follow: bool,
	///
	pub enabled_reader: bool,
	///
	pub invert_y: bool,
	///
	pub target: Option<Entity>,
}
impl Default for FlyCamera {
	fn default() -> Self {
		Self {
			accel: 1.5,
			max_speed: 0.5,
			sensitivity: 3.0,
			friction: 1.0,
			zoom_sensitivity: 0.15,
			vertical_scroll_easing_seconds: 5.0,
			horizontal_scroll_easing_seconds: 6.0,
			translation_easing_seconds: 1.0,
			rotation_easing_seconds: 1.0,
			zoom_easing_seconds: 0.01,
			lean_easing_seconds: 1.0,
			lean_reset_easing_seconds: 0.2,
			pitch: 0.0,
			yaw: 0.0,
			zoom: 10.0,
			vertical_scroll: 0.0,
			horizontal_scroll: 0.0,
			column: 40,
			row: 20,
			column_scroll_accum: 0.0,
			row_scroll_accum: 0.0,
			column_scroll_mouse_quantized: false,
			row_scroll_mouse_quantized: false,
			slowly_quantize_camera_position: true,
			slow_quatizing_easing_seconds: 0.25,
			target_translation: Vec3::ZERO,
			target_rotation: Quat::IDENTITY,
			key_scroll_delay_seconds: 0.03,
			key_scroll_delay_row_inc: 0.0,
			key_scroll_delay_row_dec: 0.0,
			key_scroll_delay_column_inc: 0.0,
			key_scroll_delay_column_dec: 0.0,
			velocity: Vec3::ZERO,
			key_forward: KeyCode::W,
			key_backward: KeyCode::S,
			key_left: KeyCode::A,
			key_right: KeyCode::D,
			key_up: KeyCode::Space,
			key_down: KeyCode::LShift,
			key_perspective: KeyCode::Return,
			mod_perspective: Some(KeyCode::LControl),
			reset_rotation_on_ortho: true,
			enabled_translation: true,
			enabled_rotation: true,
			enabled_zoom: true,
			enabled_follow: false,
			enabled_reader: true,
			invert_y: false,
			target: None,
			perspective: true,
		}
	}
}

impl FlyCamera {
	pub fn column_inc(&mut self, delta_seconds: f32) {
		self.key_scroll_delay_column_inc += delta_seconds;
		if self.key_scroll_delay_column_inc >= self.key_scroll_delay_seconds {
			self.column += 1;
			self.key_scroll_delay_column_inc -= self.key_scroll_delay_seconds;
		}
	}

	pub fn column_dec(&mut self, delta_seconds: f32) {
		self.key_scroll_delay_column_dec += delta_seconds;
		if self.column > 0 && self.key_scroll_delay_column_dec >= self.key_scroll_delay_seconds {
			self.column -= 1;
			self.key_scroll_delay_column_dec -= self.key_scroll_delay_seconds;
		}
	}

	pub fn row_inc(&mut self, delta_seconds: f32) {
		self.key_scroll_delay_row_inc += delta_seconds;
		if self.key_scroll_delay_row_inc >= self.key_scroll_delay_seconds {
			self.row += 1;
			self.key_scroll_delay_row_inc -= self.key_scroll_delay_seconds;

			let value = -1.0;
			self.pitch = value;
		}
	}

	pub fn row_dec(&mut self, delta_seconds: f32) {
		self.key_scroll_delay_row_dec += delta_seconds;
		if self.row > 0 && self.key_scroll_delay_row_dec >= self.key_scroll_delay_seconds {
			self.row -= 1;
			self.key_scroll_delay_row_dec -= self.key_scroll_delay_seconds;

			let value = 1.0;
			self.pitch = value;
		}
	}
}

fn forward_vector(rotation: &Quat) -> Vec3 {
	rotation.mul_vec3(Vec3::Z).normalize()
}

fn forward_walk_vector(rotation: &Quat) -> Vec3 {
	let f = forward_vector(rotation);
	let f_flattened = Vec3::new(f.x, 0.0, f.z).normalize();
	f_flattened
}

fn strafe_vector(rotation: &Quat) -> Vec3 {
	// Rotate it 90 degrees to get the strafe direction
	Quat::from_rotation_y(90.0f32.to_radians())
		.mul_vec3(forward_walk_vector(rotation))
		.normalize()
}

fn camera_movement_system(
	time: Res<Time>,
	keyboard_input: Res<Input<KeyCode>>,
	mut query: Query<(&mut FlyCamera, &mut Transform, &mut Projection)>,
) {
	for (mut options, mut transform, mut projection) in query.iter_mut() {
		if !options.enabled_translation || options.enabled_follow || options.enabled_reader {
			continue;
		}

		let (axis_h, axis_v, axis_float) = (
			movement_axis(&keyboard_input, options.key_right, options.key_left),
			movement_axis(
				&keyboard_input,
				options.key_backward,
				options.key_forward,
			),
			movement_axis(&keyboard_input, options.key_up, options.key_down),
		);

		let modper = options.mod_perspective;
		let perspective_mod = (modper.is_some() && keyboard_input.pressed(modper.unwrap())) || modper.is_none();
		if perspective_mod && keyboard_input.just_pressed(options.key_perspective) {
			let toggle 	= !options.perspective;
			options.perspective = toggle;

			*projection = 
			if options.perspective {
				Projection::Perspective(PerspectiveProjection::default())
			} else {
				options.yaw = 0.0;
				options.pitch = 0.0;

				Projection::Orthographic(
				OrthographicProjection {
					scale: 3.0,
					scaling_mode: ScalingMode::FixedVertical(2.0),
					..default()
				
				})
			}
		}

		let rotation = transform.rotation;
		let accel: Vec3 = (strafe_vector(&rotation) * axis_h)
			+ (forward_walk_vector(&rotation) * axis_v)
			+ (Vec3::Y * axis_float);
		let accel: Vec3 = if accel.length() != 0.0 {
			accel.normalize() * options.accel
		} else {
			Vec3::ZERO
		};

		let friction: Vec3 = if options.velocity.length() != 0.0 {
			options.velocity.normalize() * -1.0 * options.friction
		} else {
			Vec3::ZERO
		};

		options.velocity += accel * time.delta_seconds();

		// clamp within max speed
		if options.velocity.length() > options.max_speed {
			options.velocity = options.velocity.normalize() * options.max_speed;
		}

		let delta_friction = friction * time.delta_seconds();

		options.velocity = if (options.velocity + delta_friction).signum()
			!= options.velocity.signum()
		{
			Vec3::ZERO
		} else {
			options.velocity + delta_friction
		};

		transform.translation += options.velocity;
	}
}

// thanks smooth-bevy-cameras and Dunkan!
fn unit_vector_from_yaw_and_pitch(yaw: f32, pitch: f32) -> Vec3 {
    let ray = Mat3::from_rotation_y(yaw) * Vec3::Z;
    let pitch_axis = ray.cross(Vec3::Y);

    Mat3::from_axis_angle(pitch_axis, pitch) * ray
}

fn camera_follow_system(
	time: Res<Time>,
	mut mouse_motion_event_reader: EventReader<MouseMotion>,
	mut mouse_wheel_event_reader: EventReader<MouseWheel>,
	mut query_cam: Query<(&mut FlyCamera, &mut Transform)>,
		query_target: Query<&Transform, Without<FlyCamera>>,
) {
	for (mut options, mut camera_transform) in query_cam.iter_mut() {
		if !options.enabled_follow || options.target == None {
			continue;
		}

		let mut delta: Vec2 = Vec2::ZERO;
		for event in mouse_motion_event_reader.iter() {
			delta += event.delta;
		}
		if delta.is_nan() {
			continue;
		}

		if options.enabled_rotation {
			options.yaw -= delta.x * options.sensitivity * time.delta_seconds();
			options.pitch += delta.y * options.sensitivity * time.delta_seconds();

			options.pitch = options.pitch.clamp(-89.0, 89.9);
		}

		let yaw_radians = options.yaw.to_radians();
		let pitch_radians = options.pitch.to_radians();

		//

		let pixels_per_line = 53.0;
		let mut scalar = 1.0;
		for event in mouse_wheel_event_reader.iter() {
			// scale the event magnitude per pixel or per line
			let scroll_amount = match event.unit {
				MouseScrollUnit::Line => event.y,
				MouseScrollUnit::Pixel => event.y / pixels_per_line,
			};
			scalar *= 1.0 - scroll_amount * options.zoom_sensitivity;
		}

		if options.enabled_zoom {
			options.zoom = (scalar * options.zoom)
				.min(100.0)
				.max(1.0);
		}

		//
		if options.enabled_translation {
			let target = options.target.unwrap();
			let target_transform = query_target.get(target).unwrap();

			camera_transform.translation = target_transform.translation + options.zoom * unit_vector_from_yaw_and_pitch(yaw_radians, pitch_radians);
		}

		if options.enabled_rotation {
			camera_transform.rotation = Quat::from_axis_angle(Vec3::Y, yaw_radians) * Quat::from_axis_angle(-Vec3::X, pitch_radians);
		}
	}
}

fn mouse_motion_system(
	time: Res<Time>,
	mut mouse_motion_event_reader: EventReader<MouseMotion>,
	mut query: Query<(&mut FlyCamera, &mut Transform)>,
) {
	let mut delta: Vec2 = Vec2::ZERO;
	for event in mouse_motion_event_reader.iter() {
		delta += event.delta;
	}
	if delta.is_nan() {
		return;
	}

	for (mut options, mut transform) in query.iter_mut() {
		if !options.enabled_rotation || options.enabled_follow || options.enabled_reader {
			continue;
		}
		options.yaw -= delta.x * options.sensitivity * time.delta_seconds();
		options.pitch += delta.y * options.sensitivity * time.delta_seconds();

		options.pitch = options.pitch.clamp(-89.0, 89.9);
		// println!("pitch: {}, yaw: {}", options.pitch, options.yaw);

		let yaw_radians = options.yaw.to_radians();
		let pitch_radians = options.pitch.to_radians();

		transform.rotation = Quat::from_axis_angle(Vec3::Y, yaw_radians)
			* Quat::from_axis_angle(-Vec3::X, pitch_radians);
	}
}

fn mouse_reader_system(
	time: Res<Time>,
	mut mouse_motion_event_reader: EventReader<MouseMotion>,
	mut mouse_wheel_event_reader: EventReader<MouseWheel>,
	mut q_flycam: Query<(&mut FlyCamera, &mut Transform, &Children)>,
		q_center_pick_raycast: Query<&PickingObject, With<CenterPickRaycast>>,
		q_target: Query<(&Transform, &ReaderData), Without<FlyCamera>>,
		q_center_pick: Query<Entity, With<CenterPick>>,
	mut commands: Commands
) {
	let mut delta: Vec2 = Vec2::ZERO;
	for event in mouse_motion_event_reader.iter() {
		delta += event.delta;
	}
	if delta.is_nan() {
		return;
	}

	let delta_seconds = time.delta_seconds();

	for (mut options, mut camera_transform, children) in q_flycam.iter_mut() {
		if !options.enabled_reader {
			continue;
		}

		// cleanup previous CenterPick
		for e in q_center_pick.iter() {
			commands.entity(e).remove::<CenterPick>();
		}

		// check intersections on child raycaster and mark them as CenterPick
		for child in children.iter() {
			let p = q_center_pick_raycast.get(*child);
			if p.is_err() {
				continue;
			}

			let p = p.unwrap();
			if let Some(intersections) = p.intersect_list() {
				for (e_ref, _data) in intersections.iter() {
					commands.entity(*e_ref).insert(CenterPick);
				}
			}
		}

		let yaw_radians = options.yaw.to_radians();
		let pitch_radians = options.pitch.to_radians();

		if options.enabled_translation {
			let target = options.target.unwrap();
			let (target_transform, reader_data) = q_target.get(target).unwrap();

			let delta_x = delta.x;
			let delta_y = if options.invert_y { delta.y } else { -delta.y };

			options.row_scroll_accum += delta_y * (delta_seconds / options.vertical_scroll_easing_seconds);
			options.column_scroll_accum += delta_x * (delta_seconds / options.horizontal_scroll_easing_seconds);

			// we keep row_scroll_accum in range of 0..glyph_height
			while options.row_scroll_accum.abs() > reader_data.glyph_height {
				let delta_one = delta.y.signum();
				if options.row > 0 || delta_one.is_sign_positive() {
					options.row = (options.row as f32 + delta_one) as u32;
				}

				options.row_scroll_accum -= reader_data.glyph_height * options.row_scroll_accum.signum();
			}

			// we also keep row_scroll_accum in range of 0..glyph_width
			while options.column_scroll_accum.abs() > reader_data.glyph_width {
				let delta_one = delta.x.signum();
				if options.column > 0 || delta_one.is_sign_positive() {
					options.column = (options.column as f32 + delta_one) as u32;
				}

				options.column_scroll_accum -= reader_data.glyph_width * options.column_scroll_accum.signum();
			}

			let column = options.column as f32;
			let row = if options.invert_y { options.row as f32 } else { -(options.row as f32) };

			options.horizontal_scroll = column * reader_data.glyph_width;
			options.vertical_scroll = row * reader_data.glyph_height;

			if !options.column_scroll_mouse_quantized {
				options.horizontal_scroll += options.column_scroll_accum;
			}

			if !options.row_scroll_mouse_quantized {
				options.vertical_scroll += options.row_scroll_accum;
			}

			if options.slowly_quantize_camera_position { // always slowly move camera to quantized position
				let inertia = delta_seconds / options.slow_quatizing_easing_seconds;

				options.row_scroll_accum = options.row_scroll_accum.lerp(0.0, inertia);
				options.column_scroll_accum = options.column_scroll_accum.lerp(0.0, inertia);
			}

			options.target_translation = target_transform.translation
				+ options.zoom * unit_vector_from_yaw_and_pitch(yaw_radians, pitch_radians)
				+ Vec3::X * options.horizontal_scroll
				+ Vec3::Y * options.vertical_scroll
				;

			let inertia = delta_seconds / options.translation_easing_seconds;
			camera_transform.translation = camera_transform.translation.lerp(options.target_translation, 0.5);
		}

		if options.enabled_rotation {
			let value = 3.0;
			let delta_y = if options.invert_y { -delta.y } else {delta.y };
			let (target_pitch, inertia) =
			if delta_y < 0.0 {
				(-value, delta_seconds / options.lean_easing_seconds)
			} else if delta_y > 0.0 {
				(value, delta_seconds / options.lean_easing_seconds)
			} else {
				(0.0, delta_seconds / options.lean_reset_easing_seconds)
			};

			options.pitch = options.pitch.lerp(target_pitch, inertia);

			let from = camera_transform.rotation;
			let to = Quat::from_axis_angle(Vec3::X, options.pitch.to_radians());

			let inertia = delta_seconds / options.rotation_easing_seconds;
			camera_transform.rotation = from.slerp(to, inertia);
		}

		if options.enabled_zoom {
			let pixels_per_line = 53.0;
			let mut scalar = 1.0;
			for event in mouse_wheel_event_reader.iter() {
				// scale the event magnitude per pixel or per line
				let scroll_amount = match event.unit {
					MouseScrollUnit::Line => event.y,
					MouseScrollUnit::Pixel => event.y / pixels_per_line,
				};
				scalar *= 1.0 - scroll_amount * options.zoom_sensitivity;
			}

			let inertia = delta_seconds / options.zoom_easing_seconds;
			let target_zoom = (scalar * options.zoom)
				.min(100.0)
				.max(1.0);
			
			options.zoom = options.zoom.lerp(target_zoom, inertia);
		}
	}
}

fn init_camera_system(
	query: Query<(Entity, &FlyCamera), Without<CenterPickRaycastParent>>,
	mut commands: Commands
) {
	if query.is_empty() {
		return;
	}

	for (entity, _options) in query.iter() {
		commands.entity(entity)
		.with_children(|parent| {
			// rotating a child caster so that raycast points forwards, assuming there is something in front of camera
			let mut transform	= Transform::identity();
			transform.look_at	(-Vec3::Z, Vec3::Y);

			let _picking_child =
			parent.spawn_bundle(TransformBundle { local : transform, ..default() })
			.insert(PickingObject::new_transform_empty())
			.insert(CenterPickRaycast)
			.id()
			;
		})
		.insert(CenterPickRaycastParent)
		;
	}
}

/**
Include this plugin to add the systems for the FlyCamera bundle.

```no_compile
fn main() {
	App::build().add_plugin(FlyCameraPlugin);
}
```

**/

pub struct FlyCameraPlugin;

impl Plugin for FlyCameraPlugin {
	fn build(&self, app: &mut App) {
		app
			.add_system(camera_movement_system)
			.add_system(camera_2d_movement_system)
			.add_system(mouse_motion_system)
			.add_system(camera_follow_system)
			.add_system(mouse_reader_system)
			.add_system(init_camera_system)
			;
	}
}
