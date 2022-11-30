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


use bevy::{ 
	input::{
		*,
		mouse::{ MouseMotion, MouseScrollUnit, MouseWheel },
		keyboard::*,
	},
	prelude::*,
	render::camera::{*, self}
};

use bevy_mod_picking::*;
use lerp::Lerp;

#[derive(Component)]
pub struct CenterPick;

#[derive(Component)]
pub struct CenterPickRaycast;

#[derive(Component)]
pub struct CenterPickRaycastParent;

#[derive(Component, Default, Clone, Copy)]
pub struct TextDescriptor {
	pub glyph_width: f32,
	pub glyph_height: f32,
	pub rows: u32,
	pub columns: u32,
}

#[derive(Component, Default)]
pub struct Row(pub u32);

#[derive(Component, Default)]
pub struct Column(pub u32);


type PickingObject = bevy_mod_picking::RaycastSource<PickingRaycastSet>;

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
	pub slow_quantizing_easing_seconds: f32,
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
	pub pitch_changed: bool,
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
			translation_easing_seconds: 0.2,
			rotation_easing_seconds: 1.0,
			zoom_easing_seconds: 0.01,
			lean_easing_seconds: 1.0,
			lean_reset_easing_seconds: 0.2,
			pitch: 0.0,
			yaw: 0.0,
			zoom: 6.7,
			vertical_scroll: 0.0,
			horizontal_scroll: 0.0,
			column: 51,
			row: 19,
			column_scroll_accum: 0.0,
			row_scroll_accum: 0.0,
			column_scroll_mouse_quantized: false,
			row_scroll_mouse_quantized: false,
			slowly_quantize_camera_position: true,
			slow_quantizing_easing_seconds: 0.25,
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
			perspective: true,
			enabled_translation: true,
			enabled_rotation: true,
			enabled_zoom: true,
			enabled_follow: false,
			enabled_reader: false,
			invert_y: false,
			pitch_changed: false,
			target: None,
		}
	}
}

impl FlyCamera {
	pub fn set_pitch(&mut self, pitch: f32) {
		self.pitch = pitch;
		self.pitch_changed = true;
	}

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

		self.set_pitch(-1.0);

		if self.key_scroll_delay_row_inc >= self.key_scroll_delay_seconds {
			self.row += 1;
			self.key_scroll_delay_row_inc -= self.key_scroll_delay_seconds;
		}

	}

	pub fn row_dec(&mut self, delta_seconds: f32) {
		self.key_scroll_delay_row_dec += delta_seconds;

		self.set_pitch(1.0);

		if self.row > 0 && self.key_scroll_delay_row_dec >= self.key_scroll_delay_seconds {
			self.row -= 1;
			self.key_scroll_delay_row_dec -= self.key_scroll_delay_seconds;
		}
	}
}

pub fn movement_axis(
	input: &Res<Input<KeyCode>>,
	plus: KeyCode,
	minus: KeyCode,
) -> f32 {
	let mut axis = 0.0;
	if input.pressed(plus) {
		axis += 1.0;
	}
	if input.pressed(minus) {
		axis -= 1.0;
	}
	axis
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
		time			: Res<Time>,
		key_code		: Res<Input<KeyCode>>,
	mut q_fly_camera	: Query<(&mut FlyCamera, &mut Transform, &mut Projection)>,
) {
	for (mut fly_camera, mut camera_transform, mut projection) in q_fly_camera.iter_mut() {
		if !fly_camera.enabled_translation || fly_camera.enabled_follow || fly_camera.enabled_reader {
			continue;
		}

		let (axis_h, axis_v, axis_float) = (
			movement_axis(&key_code, fly_camera.key_right,		fly_camera.key_left),
			movement_axis(&key_code, fly_camera.key_backward,	fly_camera.key_forward),
			movement_axis(&key_code, fly_camera.key_up,			fly_camera.key_down),
		);

		let modper = fly_camera.mod_perspective;
		let perspective_mod = (modper.is_some() && key_code.pressed(modper.unwrap())) || modper.is_none();
		if perspective_mod && key_code.just_pressed(fly_camera.key_perspective) {
			let toggle 	= !fly_camera.perspective;
			fly_camera.perspective = toggle;

			*projection = 
			if fly_camera.perspective {
				Projection::Perspective(PerspectiveProjection::default())
			} else {
				fly_camera.yaw = 0.0;
				fly_camera.pitch = 0.0;

				Projection::Orthographic(
				OrthographicProjection {
					scale: 3.0,
					scaling_mode: ScalingMode::FixedVertical(2.0),
					..default()
				
				})
			}
		}

		let rotation = camera_transform.rotation;
		let accel: Vec3 = (strafe_vector(&rotation) * axis_h)
			+ (forward_walk_vector(&rotation) * axis_v)
			+ (Vec3::Y * axis_float);
		let accel: Vec3 = if accel.length() != 0.0 {
			accel.normalize() * fly_camera.accel
		} else {
			Vec3::ZERO
		};

		let friction: Vec3 = if fly_camera.velocity.length() != 0.0 {
			fly_camera.velocity.normalize() * -1.0 * fly_camera.friction
		} else {
			Vec3::ZERO
		};

		fly_camera.velocity += accel * time.delta_seconds();

		// clamp within max speed
		if fly_camera.velocity.length() > fly_camera.max_speed {
			fly_camera.velocity = fly_camera.velocity.normalize() * fly_camera.max_speed;
		}

		let delta_friction = friction * time.delta_seconds();

		fly_camera.velocity = if (fly_camera.velocity + delta_friction).signum()
			!= fly_camera.velocity.signum()
		{
			Vec3::ZERO
		} else {
			fly_camera.velocity + delta_friction
		};

		camera_transform.translation += fly_camera.velocity;
	}
}

// thanks smooth-bevy-cameras and Dunkan!
fn unit_vector_from_yaw_and_pitch(yaw: f32, pitch: f32) -> Vec3 {
    let ray = Mat3::from_rotation_y(yaw) * Vec3::Z;
    let pitch_axis = ray.cross(Vec3::Y);

    Mat3::from_axis_angle(pitch_axis, pitch) * ray
}

fn camera_follow_system(
		time						: Res<Time>,
	mut mouse_motion_event_reader	: EventReader<MouseMotion>,
	mut mouse_wheel_event_reader	: EventReader<MouseWheel>,
	mut q_fly_camera				: Query<(&mut FlyCamera, &mut Transform)>,
		q_target					: Query<&Transform, Without<FlyCamera>>,
) {
	for (mut fly_camera, mut camera_transform) in q_fly_camera.iter_mut() {
		if !fly_camera.enabled_follow || fly_camera.target == None {
			continue;
		}

		let mut delta: Vec2 = Vec2::ZERO;
		for event in mouse_motion_event_reader.iter() {
			delta += event.delta;
		}
		if delta.is_nan() {
			continue;
		}

		if fly_camera.enabled_rotation {
			fly_camera.yaw -= delta.x * fly_camera.sensitivity * time.delta_seconds();
			fly_camera.pitch += delta.y * fly_camera.sensitivity * time.delta_seconds();

			fly_camera.pitch = fly_camera.pitch.clamp(-89.0, 89.9);
		}

		let yaw_radians = fly_camera.yaw.to_radians();
		let pitch_radians = fly_camera.pitch.to_radians();

		//

		let pixels_per_line = 53.0;
		let mut scalar = 1.0;
		for event in mouse_wheel_event_reader.iter() {
			// scale the event magnitude per pixel or per line
			let scroll_amount = match event.unit {
				MouseScrollUnit::Line => event.y,
				MouseScrollUnit::Pixel => event.y / pixels_per_line,
			};
			scalar *= 1.0 - scroll_amount * fly_camera.zoom_sensitivity;
		}

		if fly_camera.enabled_zoom {
			fly_camera.zoom = (scalar * fly_camera.zoom)
				.min(100.0)
				.max(1.0);
		}

		//
		if fly_camera.enabled_translation {
			let target = fly_camera.target.unwrap();
			let target_transform = q_target.get(target).unwrap();

			camera_transform.translation = target_transform.translation + fly_camera.zoom * unit_vector_from_yaw_and_pitch(yaw_radians, pitch_radians);
		}

		if fly_camera.enabled_rotation {
			camera_transform.rotation = Quat::from_axis_angle(Vec3::Y, yaw_radians) * Quat::from_axis_angle(-Vec3::X, pitch_radians);
		}
	}
}

fn mouse_motion_system(
	time: Res<Time>,
	mut mouse_motion_event_reader: EventReader<MouseMotion>,
	mut q_fly_camera: Query<(&mut FlyCamera, &mut Transform)>,
) {
	let mut delta: Vec2 = Vec2::ZERO;
	for event in mouse_motion_event_reader.iter() {
		delta += event.delta;
	}
	if delta.is_nan() {
		return;
	}

	for (mut fly_camera, mut transform) in q_fly_camera.iter_mut() {
		if !fly_camera.enabled_rotation || fly_camera.enabled_follow || fly_camera.enabled_reader {
			continue;
		}
		fly_camera.yaw -= delta.x * fly_camera.sensitivity * time.delta_seconds();
		fly_camera.pitch += delta.y * fly_camera.sensitivity * time.delta_seconds();

		fly_camera.pitch = fly_camera.pitch.clamp(-89.0, 89.9);
		// println!("pitch: {}, yaw: {}", options.pitch, options.yaw);

		let yaw_radians = fly_camera.yaw.to_radians();
		let pitch_radians = fly_camera.pitch.to_radians();

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
		q_target: Query<(&Transform, &TextDescriptor), Without<FlyCamera>>,
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

	for (mut fly_camera, mut camera_transform, children) in q_flycam.iter_mut() {
		if !fly_camera.enabled_reader {
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
			for (e_ref, _data) in p.intersections().iter() {
				commands.entity(*e_ref).insert(CenterPick);
			}
		}

		let yaw_radians = fly_camera.yaw.to_radians();
		let pitch_radians = fly_camera.pitch.to_radians();

		// translation
		{
			let target = fly_camera.target.unwrap();
			let (target_transform, text_descriptor) = q_target.get(target).unwrap();

			let delta_x = delta.x;
			let delta_y = delta.y;

			fly_camera.row_scroll_accum += delta_y * (delta_seconds / fly_camera.vertical_scroll_easing_seconds);
			fly_camera.column_scroll_accum += delta_x * (delta_seconds / fly_camera.horizontal_scroll_easing_seconds);

			// we keep row_scroll_accum in range of 0..glyph_height
			while fly_camera.row_scroll_accum.abs() > text_descriptor.glyph_height {
				let delta_one = delta.y.signum();
				if fly_camera.enabled_translation && (fly_camera.row > 0 || delta_one.is_sign_positive()) {
					fly_camera.row = (fly_camera.row as f32 + delta_one) as u32;
					// clamping
					fly_camera.row = fly_camera.row.min(text_descriptor.rows);
				}

				fly_camera.row_scroll_accum -= text_descriptor.glyph_height * fly_camera.row_scroll_accum.signum();
			}

			// we also keep row_scroll_accum in range of 0..glyph_width
			while fly_camera.column_scroll_accum.abs() > text_descriptor.glyph_width {
				let delta_one = delta.x.signum();
				if fly_camera.enabled_translation && (fly_camera.column > 0 || delta_one.is_sign_positive()) {
					fly_camera.column = (fly_camera.column as f32 + delta_one) as u32;
					// clamping
					fly_camera.column = fly_camera.column.min(text_descriptor.columns * 2);
				}

				fly_camera.column_scroll_accum -= text_descriptor.glyph_width * fly_camera.column_scroll_accum.signum();
			}

			let column = fly_camera.column as f32;
			let row = fly_camera.row as f32;

			fly_camera.horizontal_scroll = column * text_descriptor.glyph_width;
			fly_camera.vertical_scroll = row * text_descriptor.glyph_height;

			if !fly_camera.column_scroll_mouse_quantized {
				fly_camera.horizontal_scroll += fly_camera.column_scroll_accum;
			}

			if !fly_camera.row_scroll_mouse_quantized {
				fly_camera.vertical_scroll += fly_camera.row_scroll_accum;
			}

			if fly_camera.slowly_quantize_camera_position { // always slowly move camera to quantized position
				let inertia = (delta_seconds / fly_camera.slow_quantizing_easing_seconds).min(1.0);

				fly_camera.row_scroll_accum = fly_camera.row_scroll_accum.lerp(0.0, inertia);
				fly_camera.column_scroll_accum = fly_camera.column_scroll_accum.lerp(0.0, inertia);
			}

			let vertical_scroll = if fly_camera.invert_y { fly_camera.vertical_scroll } else { -fly_camera.vertical_scroll };

			fly_camera.target_translation = target_transform.translation
				+ fly_camera.zoom * unit_vector_from_yaw_and_pitch(yaw_radians, pitch_radians)
				+ Vec3::X * fly_camera.horizontal_scroll
				+ Vec3::Y * vertical_scroll
				;

			let inertia = (delta_seconds / fly_camera.translation_easing_seconds).min(1.0);
			camera_transform.translation = camera_transform.translation.lerp(fly_camera.target_translation, inertia);
		}

		if fly_camera.enabled_rotation {
			let value = 3.0;
			let delta_y = if fly_camera.invert_y { -delta.y } else { delta.y };
			let (target_pitch, inertia) =
			if delta_y < 0.0 {
				(-value, delta_seconds / fly_camera.lean_easing_seconds)
			} else if delta_y > 0.0 {
				(value, delta_seconds / fly_camera.lean_easing_seconds)
			} else if fly_camera.pitch_changed {
				(fly_camera.pitch, delta_seconds / fly_camera.lean_easing_seconds)
			} else {
				(0.0, delta_seconds / fly_camera.lean_reset_easing_seconds)
			};

			fly_camera.pitch = fly_camera.pitch.lerp(target_pitch, inertia);

			let from = camera_transform.rotation;
			let to = Quat::from_axis_angle(Vec3::X, fly_camera.pitch.to_radians());

			let inertia = (delta_seconds / fly_camera.rotation_easing_seconds).min(1.0);
			camera_transform.rotation = from.slerp(to, inertia);
		}

		if fly_camera.enabled_zoom {
			let pixels_per_line = 53.0;
			let mut scalar = 1.0;
			for event in mouse_wheel_event_reader.iter() {
				// scale the event magnitude per pixel or per line
				let scroll_amount = match event.unit {
					MouseScrollUnit::Line => event.y,
					MouseScrollUnit::Pixel => event.y / pixels_per_line,
				};
				scalar *= 1.0 - scroll_amount * fly_camera.zoom_sensitivity;
			}

			let inertia = (delta_seconds / fly_camera.zoom_easing_seconds).min(1.0);
			let target_zoom = (scalar * fly_camera.zoom)
				.min(100.0)
				.max(1.0);

			fly_camera.zoom = fly_camera.zoom.lerp(target_zoom, inertia);
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
			let mut transform	= Transform::default();
			transform.look_at	(-Vec3::Z, Vec3::Y);

			let _picking_child =
			parent.spawn(TransformBundle { local : transform, ..default() })
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
			.add_system(init_camera_system)
			.add_system(camera_movement_system)
			.add_system(camera_follow_system)
			.add_system(mouse_motion_system)
			.add_system(mouse_reader_system)
			
			;
	}
}
