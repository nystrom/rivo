pub mod graph;
pub mod namer;
pub mod prename;
pub mod rename;
pub mod symbols;

mod earley;
mod worklist;

// mod cheap_renamer;

// mod play {
//     use datafrog::Iteration;

//     fn foo() {
//         // Create a new iteration context, ...
//         let mut iteration = Iteration::new();

//         // .. some variables, ..
//         let nodes_var = iteration.variable::<(u32, u32)>("nodes");
//         let edges_var = iteration.variable::<(u32, u32)>("edges");

//         let nodes = vec![(1, 2), (3, 4), (5, 6)];
//         let edges = vec![(1, 2), (2, 6)];
//         // .. load them with some initial values, ..
//         nodes_var.insert(nodes.into());
//         edges_var.insert(edges.into());

//         // .. and then start iterating rules!
//         while iteration.changed() {
//             // nodes(a,c)  <-  nodes(a,b), edges(b,c)
//             nodes_var.from_join(&nodes_var, &edges_var, |_b, &a, &c| (c, a));
//         }

//         // extract the final results.
//         let reachable: Vec<(u32, u32)> = nodes_var.complete();
//     }
// }
