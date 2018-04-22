defmodule ScratchTest do
  use ExUnit.Case
  doctest Scratch

  test "offset is 0, long sentence" do
    a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    {key, lhs, rhs, rev_lhs} = Scratch.test(a, 0)
    assert key == 0
    assert lhs == []
    assert rev_lhs == []
    assert rhs == [1, 2, 3]
  end


  test "offset is 0, short sentence" do
      a = [0, 1, 2]

      {key, lhs, rhs, rev_lhs} = Scratch.test(a, 0)
      assert key == 0
      assert lhs == []
      assert rev_lhs == []
      assert rhs == [1, 2]
  end


  test "offset is 2, short sentence" do
      a = [0, 1, 2]

      {key, lhs, rhs, rev_lhs} = Scratch.test(a, 2)
      assert key == 2
      assert lhs == [0, 1]
      assert rev_lhs == [1, 0]
      assert rhs == []
    end


  test "offset is more than 3" do
    a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    {key, lhs, rhs, rev_lhs} = Scratch.test(a, 5)
    assert key == 5
    assert lhs == [2, 3, 4]
    assert rev_lhs == [4, 3, 2]
    assert rhs == [6, 7, 8]
  end


    test "offset is before end" do
      a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      {key, lhs, rhs, rev_lhs} = Scratch.test(a, 7)
      assert key == 7
      assert lhs == [4, 5, 6]
      assert rev_lhs == [6, 5, 4]
      assert rhs == [8, 9, 10]
    end


      test "offset is near end" do
        a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        {key, lhs, rhs, rev_lhs} = Scratch.test(a, 8)
        assert key == 8
        assert lhs == [5, 6, 7]
        assert rev_lhs == [7, 6, 5]
        assert rhs == [9, 10]
      end


            test "offset is at end" do
              a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

              {key, lhs, rhs, rev_lhs} = Scratch.test(a, 10)
              assert key == 10
              assert lhs == [7, 8, 9]
              assert rev_lhs == [9, 8, 7]
              assert rhs == []
            end
end
