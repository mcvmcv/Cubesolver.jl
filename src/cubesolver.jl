module Cubesolver


import Base: ==, hash
using Combinatorics


notx(x) = i -> i != x
notx(x::AbstractArray) = i -> i != only(x)


origin(d) = ntuple(_ -> 1, d)

function orient(blueprint::Vector{Vector{Int}}, axis::Vector{Int}, orientation::Vector{Int})
    [((s .* orientation)[axis]) for s in blueprint]
end

function all_orientations(blueprint::Vector{Vector{Int}})
    d::Int = length(blueprint[1])
    axes = permutations(1:d)
    orientations = collect.(Iterators.product(ntuple(_ -> (-1,1), d)...))
    (orient(blueprint, axis, orientation) for axis in axes for orientation in orientations)
end

function place(orientedblueprint::Vector{Vector{Int}}, origin::Vector{Int})
    sort!([origin + b for b in orientedblueprint])
end

function max_origin_indices(blueprint::Vector{Vector{Int}}, puzzlesize::Int)
    puzzlesize .- [maximum([step[d] for step in blueprint]) for d in eachindex(blueprint[1])]
end

function max_origin_indices(blueprint::Vector{Vector{Int}}, puzzlesize::Vector{Int})
    puzzlesize - [maximum([step[d] for step in blueprint]) for d in eachindex(puzzlesize)]
end

function min_origin_indices(blueprint::Vector{Vector{Int}})
    -[minimum([step[d] for step in blueprint]) for d in eachindex(blueprint[1])] .+ 1
end

function vectors_between(x, y)
    collect.(Iterators.product((i:j for (i,j) in zip(x, y))...))
end

#function vectors_between(x, y)
#    Iterators.product((i:j for (i,j) in zip(x, y))...)
#end

function allowed_origins(blueprint::Vector{Vector{Int}}, puzzlesize)
    vectors_between(min_origin_indices(blueprint), max_origin_indices(blueprint, puzzlesize))
end

function generate_pieces(blueprint::Vector{Vector{Int}}, puzzlesize::Vector{Int})
    pieces = Vector{Vector{Vector{Int}}}()
    for orientedblueprint in all_orientations(blueprint)
        origins = allowed_origins(orientedblueprint, puzzlesize)
        for origin in origins
            push!(pieces, place(orientedblueprint, origin))
        end
    end
    unique(pieces)
end

function make_puzzle(blueprint::Vector{Vector{Int}}, puzzlesize::Vector{Int})
    cubes = vectors_between(ones(Int, length(puzzlesize)), puzzlesize)
    pieces = generate_pieces(blueprint, puzzlesize)
    Dict(cube => filter(piece -> in(cube, piece), pieces) for cube in cubes)
end

function make_int_puzzle(blueprint::Vector{Vector{Int}}, puzzlesize::Vector{Int})
    cubes = vectors_between(ones(Int, length(puzzlesize)), puzzlesize)[:]
    pieces = generate_pieces(blueprint, puzzlesize)
    int_pieces = [sort!([findfirst(x -> x==cube, cubes) for cube in piece]) for piece in pieces]
    int_puzzle = [sort!(filter(x -> in(i, x), int_pieces)) for i=1:length(cubes)]
end


function assign(puzzle::Vector{Vector{Vector{Int}}}, piece::Vector{Int})
    for cube in piece
        for other_piece in puzzle[cube]
            if other_piece != piece
                (puzzle = eliminate(puzzle, other_piece)) ==  Vector{Vector{Vector{Int}}}() && return Vector{Vector{Vector{Int}}}()
            end
        end
        

        #other_pieces = filter(p -> p != piece, puzzle[cube])
        #for other_piece in other_pieces
        #    (puzzle = eliminate(puzzle, other_piece)) == false && return false
        #end
    end
    return puzzle
end

function eliminate(puzzle::Vector{Vector{Vector{Int}}}, piece::Vector{Int})
    #all(!in(piece, puzzle[cube]) for cube in piece) && return puzzle
    
    for cube in piece
        if in(piece, puzzle[cube])
            break
        end
        return puzzle
    end

    for cube in piece
        puzzle[cube] = [p for p in puzzle[cube] if p != piece]
#        puzzle[cube] = filter(notx(piece), puzzle[cube])
        if isempty(puzzle[cube])
            return Vector{Vector{Vector{Int}}}()
        elseif length(puzzle[cube]) == 1
            (puzzle = assign(puzzle, only(puzzle[cube]))) == Vector{Vector{Vector{Int}}}() && return Vector{Vector{Vector{Int}}}()
        end
    end

    return puzzle
end

function search(puzzle)
    "Using depth-first search and propagation, try all possible values."
    if puzzle == false
        return false ## Failed earlier
    elseif all(length(puzzle[cube]) == 1 for cube in keys(puzzle))
    #elseif !any(length(puzzle[cube]) != 1 for cube in keys(puzzle))
        return puzzle ## Solved!
    end
    ## Choose the unfilled square s with the fewest possibilities
    n, cube = minimum((length(puzzle[cube]), cube) for cube in keys(puzzle) if length(puzzle[cube]) > 1)
    return some(search(assign(copy(puzzle), piece)) for piece in puzzle[cube])
end

function some(seq::Vector{Vector{Vector{Int}}})
    for e in seq
        !isempty(e) && return e
    end
    return Vector{Vector{Vector{Int}}}()
end    

function sometrue(seq)
    "Return some element of seq that is true."
    for e in seq
        e != false && return e
    end
    return false
end

function solve(blueprint, size)
    sort(unique(only.(search(make_int_puzzle(blueprint, size)))))
end
    

d = 3
s = 5
p = [5, 5, 5]


origin5 = origin(5)
axes5 = permutations(1:d)
orient5 = Iterators.product(ntuple(_ -> (-1,1), d)...)







    

end
