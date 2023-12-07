import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
from matplotlib.animation import FuncAnimation, writers
import matplotlib.cm as cm
from IPython.display import Video
import multiprocessing
import time
start_time = time.time()

def lorentz_system(t, xyz, params):
    x, y, z = xyz
    sigma, rho, beta = params['sigma'], params['rho'], params['beta']
    return [sigma * (y - x), x * (rho - z) - y, x * y - beta * z]

def rossler_system(t, xyz, params):
    x, y, z = xyz
    a, b, c = params['a'], params['b'], params['c']
    return [-y - z, x + a * y, b + z * (x - c)]

def chen_system(t, xyz, params):
    x, y, z = xyz
    a, b, c = params['a'], params['b'], params['c']
    return [a * x + y * z, b * x + c * y, -z - x * y]

def compute_trajectory(args):
    system_function, initial_conditions, params = args
    t_eval = np.linspace(params['t_span'][0], params['t_span'][1], params['t_steps'])
    return solve_ivp(system_function, params['t_span'], initial_conditions, args=(params, ), t_eval=t_eval).y

def animate_attractor(i, trajectories, lines, dots, params, ax):
    t_steps, particles_per_trajectory = params['t_steps'], params['particles_per_trajectory']
    for trajectory, line, dot_group in zip(trajectories, lines, dots):
        line.set_data(trajectory[0:2, :i])
        line.set_3d_properties(trajectory[2, :i])
        for j, dot in enumerate(dot_group):
            index = (i + j * t_steps // particles_per_trajectory) % t_steps
            dot.set_data(trajectory[0, index], trajectory[1, index])
            dot.set_3d_properties(trajectory[2, index])
    ax.view_init(elev=30, azim=3*i)

def plot_attractor(ax, system_function, initial_conditions_list, params, title, custom_xlim=None, custom_ylim=None, custom_zlim=None):
    with multiprocessing.Pool() as pool:
        trajectories = pool.map(compute_trajectory, [(system_function, init_cond, params) for init_cond in initial_conditions_list])

    ax.set(xlabel="X Axis", ylabel="Y Axis", zlabel="Z Axis", title=title)
    ax.xaxis.labelpad, ax.yaxis.labelpad, ax.zaxis.labelpad = 15, 15, 15
    ax.title.set_position([.5, 1.05])

    colors = cm.viridis(np.linspace(0, 1, len(initial_conditions_list)))
    lines = [ax.plot([], [], [], lw=1.5, alpha=0.7, color=color)[0] for color in colors]
    dots = [[ax.plot([], [], [], marker='o', markersize=6, alpha=0.7, color=color)[0] for _ in range(params['particles_per_trajectory'])] for color in colors]

    ax.set_xlim(custom_xlim or (min(min(traj[0]) for traj in trajectories), max(max(traj[0]) for traj in trajectories)))
    ax.set_ylim(custom_ylim or (min(min(traj[1]) for traj in trajectories), max(max(traj[1]) for traj in trajectories)))
    ax.set_zlim(custom_zlim or (min(min(traj[2]) for traj in trajectories), max(max(traj[2]) for traj in trajectories)))

    ani = FuncAnimation(fig, animate_attractor, frames=params['t_steps'], fargs=(trajectories, lines, dots, params, ax), interval=params['animation_speed'], blit=False)
    return ani



initial_conditions_list = [
    [1, 1, 1],
    [5, 5, 5],
    [10, 10, 10],
    [15, 15, 5],
    [20, 20, 20]
]


lorentz_params = {
'sigma': 10,
'rho': 28,
'beta': 8/3,
't_span': (0, 50),
't_steps': 100,
'animation_speed': 2,
'particles_per_trajectory': 5
}

rossler_params = {
'a': 0.2,
'b': 0.2,
'c': 5.7,
't_span': (0, 50),
't_steps': 100,
'animation_speed': 2,
'particles_per_trajectory': 5
}

chen_params = {
    'a': 0.2,
    'b': 0.01,
    'c': -0.4,
    't_span': (0, 50),
    't_steps': 100,
    'animation_speed': 2,
    'particles_per_trajectory': 5
}


fig = plt.figure(figsize=(20, 8))
fig.subplots_adjust(wspace=0, hspace=0)
fig.patch.set_facecolor('black')

ax1 = fig.add_subplot(1, 3, 1, projection='3d')
ax2 = fig.add_subplot(1, 3, 2, projection='3d')
ax3 = fig.add_subplot(1, 3, 3, projection='3d')

for ax in [ax1, ax2, ax3]:
    ax.grid(False)
    ax.xaxis.pane.fill = False
    ax.yaxis.pane.fill = False
    ax.zaxis.pane.fill = False
    ax.set_facecolor('black')
    ax.xaxis.pane.set_edgecolor('black')
    ax.yaxis.pane.set_edgecolor('black')
    ax.zaxis.pane.set_edgecolor('black')
    ax.xaxis.pane.set_alpha(0.5)
    ax.yaxis.pane.set_alpha(0.5)
    ax.zaxis.pane.set_alpha(0.5)
    ax.xaxis.line.set_color("black")
    ax.yaxis.line.set_color("black")
    ax.zaxis.line.set_color("black")
    ax.xaxis.label.set_color('black')
    ax.yaxis.label.set_color('black')
    ax.zaxis.label.set_color('black')
    ax.tick_params(colors='black')
    ax.title.set_color('white')

# Animation
lorentz_ani = plot_attractor(ax1, lorentz_system, initial_conditions_list, lorentz_params, "Animated Lorentz Attractor")
rossler_ani = plot_attractor(ax2, rossler_system, initial_conditions_list, rossler_params, "Animated RÃ¶ssler Attractor", custom_xlim=(-25, 25), custom_ylim=(-35, 35), custom_zlim=(0, 100))
chen_ani = plot_attractor(ax3, chen_system, initial_conditions_list, chen_params, "Animated Chen Attractor")

# Create a function to update all attractors simultaneously
def update_all(i):
    animate_attractor(i, *lorentz_ani._args)
    animate_attractor(i, *rossler_ani._args)
    animate_attractor(i, *chen_ani._args)
    return []

# Create the combined animation
combined_ani = FuncAnimation(fig, update_all, frames=lorentz_params['t_steps'], interval=lorentz_params['animation_speed'], blit=True)

# Save the combined animation as an MP4 file
Writer = writers['ffmpeg']
writer = Writer(fps=15, metadata=dict(artist='Me'), bitrate=1800)
combined_ani.save('combined_attractors.mp4', writer=writer)

# Display the combined video clip
Video("combined_attractors.mp4")

end_time = time.time()
elapsed_time = end_time - start_time
print(f"Elapsed time: {elapsed_time:.2f} seconds")